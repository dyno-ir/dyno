import lldb

class SmallVecSynthProvider:
    def __init__(self, valobj, internal_dict):
        self.valobj = valobj
        #self.update()

    def update(self):
        self.data_ptr = self.valobj.GetChildMemberWithName("arr")
        self.size = self.valobj.GetChildMemberWithName("sz").GetValueAsUnsigned()
        self.capacity = self.valobj.GetChildMemberWithName("cap").GetValueAsUnsigned()

        # Detect element type (T)
        self.elem_type = self.data_ptr.GetType().GetPointeeType()
        self.elem_size = self.elem_type.GetByteSize()

        # Current data pointer address
        self.data_addr = self.data_ptr.GetValueAsUnsigned()


    def num_children(self):
        return int(self.size)

    def get_child_at_index(self, index):
        if index >= self.num_children():
            return None

        # base address = either inline buffer or heap
        base_addr = self.data_addr

        return self.data_ptr.CreateChildAtOffset(
            f"[{index}]",
            index * self.elem_size,
            self.elem_type
        )

    def get_child_index(self, name):
        try:
            if name.startswith("[") and name.endswith("]"):
                return int(name[1:-1])
        except:
            return -1
        return -1

    def has_children(self):
        return self.size > 0

def smallvec_summary(valobj, internal_dict):
    size = valobj.GetChildMemberWithName("sz").GetValueAsUnsigned()
    cap  = valobj.GetChildMemberWithName("cap").GetValueAsUnsigned()
    return f"SmallVec size={size} cap={cap}"


def _get_type_args(type_name):
    depth = 0
    for i, c in enumerate(type_name):
        if c == '<' and depth == 0:
            start = i + 1
        elif c == '>' and depth == 0:
            inner = type_name[start:i]
            args = []
            d = 0
            current = 0
            for j, ch in enumerate(inner):
                if ch == '<':
                    d += 1
                elif ch == '>':
                    d -= 1
                elif ch == ',' and d == 0:
                    args.append(inner[current:j].strip())
                    current = j + 1
            if current < len(inner):
                args.append(inner[current:].strip())
            return args
        elif c == '<':
            depth += 1
        elif c == '>':
            depth -= 1
    return None


def _get_addr(valobj):
    """Get the memory address of a value object."""
    addr = valobj.GetLoadAddress()
    if addr != lldb.LLDB_INVALID_ADDRESS:
        return addr
    frame = valobj.GetFrame()
    if frame:
        name = valobj.GetName()
        if name:
            e = frame.EvaluateExpression('&' + name)
            if e.IsValid():
                return e.GetValueAsUnsigned()
    return 0


class PointersIntsVariantSynthProvider:
    def __init__(self, valobj, internal_dict):
        self.valobj = valobj
        self.type_args = _get_type_args(valobj.GetType().GetName()) or []
        self.name = valobj.GetName()

        # Read raw value by accessing the 'base' member
        self.raw = 0
        base = valobj.GetChildMemberWithName("base")
        if base.IsValid():
            raw = base.GetValueAsUnsigned()
            if raw:
                self.raw = raw
            else:
                # Try getting address of value and reading from there
                addr = _get_addr(valobj)
                if addr:
                    e = valobj.EvaluateExpression(f"*((uintptr_t*){addr})")
                    self.raw = e.GetValueAsUnsigned() if e.IsValid() else 0
        else:
            addr = _get_addr(valobj)
            if addr:
                e = valobj.EvaluateExpression(f"*((uintptr_t*){addr})")
                self.raw = e.GetValueAsUnsigned() if e.IsValid() else 0

        self.type_idx = (self.raw >> 1) & 1

        active = self.type_args[self.type_idx] if self.type_idx < len(self.type_args) else ""
        self.is_ptr = active.endswith('*')

    def num_children(self):
        return 1

    def get_child_at_index(self, index):
        if index != 0 or not self.type_args:
            return None
        active = self.type_args[self.type_idx]
        child_name = active.rstrip('*').strip() if active.endswith('*') else active

        if self.is_ptr:
            ptr_addr = self.raw & ~3
            child = self.valobj.CreateValueFromExpression(
                child_name, f"*(({child_name}*)(void*){ptr_addr})"
            )
        else:
            enum_val = self.raw >> 2
            child = self.valobj.CreateValueFromExpression(
                child_name, f"(({active}){enum_val})"
            )

        return child if child.IsValid() else None

    def get_child_index(self, name):
        if not self.type_args:
            return -1
        active = self.type_args[self.type_idx]
        child_name = active.rstrip('*').strip() if active.endswith('*') else active
        return 0 if name == child_name else -1

    def has_children(self):
        return True


def pointersintsvariant_summary(valobj, internal_dict):
    type_args = _get_type_args(valobj.GetType().GetName()) or []
    if not type_args:
        return ""

    # Read raw value from the object
    raw_val = 0
    base = valobj.GetChildMemberWithName("base")
    if base.IsValid():
        raw_val = base.GetValueAsUnsigned()
    if not raw_val:
        addr = _get_addr(valobj)
        if addr:
            e = valobj.EvaluateExpression(f"*((uintptr_t*){addr})")
            raw_val = e.GetValueAsUnsigned() if e.IsValid() else 0

    type_idx = (raw_val >> 1) & 1
    active = type_args[type_idx] if type_idx < len(type_args) else ""

    if active.endswith('*'):
        ptr_val = raw_val & ~3
        if ptr_val:
            return f"PointersIntsVariant({active}=0x{ptr_val:x})"
        else:
            return f"PointersIntsVariant({active}=nullptr)"
    else:
        enum_val = raw_val >> 2
        return f"PointersIntsVariant({active}={enum_val})"


class DenseMapSynthProvider:
    def __init__(self, valobj, internal_dict):
        self.valobj = valobj
        #self.update()

    def update(self):

        self.data_ptr = self.valobj.GetChildMemberWithName("buckets")
        self.size = self.valobj.GetChildMemberWithName("sz").GetValueAsUnsigned()
        self.capacity = self.valobj.GetChildMemberWithName("cap").GetValueAsUnsigned()


        # Detect element type (T)
        self.bucket_type = self.data_ptr.GetType().GetPointeeType()
        self.bucket_size = self.bucket_type.GetByteSize()

        # Current data pointer address
        self.data_addr = self.data_ptr.GetValueAsUnsigned()

        bucket = self.data_ptr.CreateChildAtOffset(
          f"[{0}]",
          0,
          self.bucket_type
        )
        #print(f"updated3")
        #print(f"{type(bucket)} vs {type(self.valobj)}")
        self.entries_per_bucket = bucket.EvaluateExpression("entriesPerBucket").GetValueAsUnsigned()
        print(f"{self.entries_per_bucket=:}")
        self.invalid = bucket.EvaluateExpression("emptyKey").GetValueAsUnsigned()
        print(f"{self.invalid=:}")
        self.tombstone = bucket.EvaluateExpression("tombstoneKey").GetValueAsUnsigned()
        print(f"{self.tombstone=:}")



    def num_children(self):
        print(f"{self.size=:}")
        return int(self.size)

    def get_child_at_index(self, index):
        print(f"get {index=:}")
        if index >= self.num_children():
            return None

        bucket_idx = index // self.entries_per_bucket
        base_addr = self.data_addr

        bucket = self.data_ptr.CreateChildAtOffset(
            f"[{index // self.entries_per_bucket}]",
            index * self.bucket_size,
            self.bucket_type
        )
        print("abc")

        key_valid = bucket.EvaluateExpression("Info::isEqual(keys[0], emptyKey)")
        print(f"{key_valid=:}")

        values_ptr = bucket.EvaluateExpression("&values[0]")
        print(f"{type(values_ptr)=:} vs {type(self.data_ptr)=:}")
        value_type = values_ptr.GetType().GetPointeeType()
        value_size = value_type.GetByteSize()
        print("abcd")

        return values_ptr.CreateChildAtOffset(
            f"[ {index}]",
            0,#(index % self.entries_per_bucket) * value_size,
            value_type
        )

    def get_child_index(self, name):
        try:
            if name.startswith("[") and name.endswith("]"):
                return int(name[1:-1])
        except:
            return -1
        return -1

    def has_children(self):
        return self.size > 0


def bigint_summary(valobj, internal_dict):
    name = valobj.GetName()
    if not name:
        return ""
    expr_name = name.lstrip('*')
    str_result = valobj.EvaluateExpression(f"({expr_name}).toString(16, false)")
    if str_result.IsValid():
        summary = str_result.GetSummary()
        if summary:
            if summary.startswith('"') and summary.endswith('"'):
                summary = summary[1:-1]
            return summary
    return ""


def __lldb_init_module(debugger, internal_dict):
    print("loading SmallVec vis function")
    debugger.HandleCommand('type synthetic add -l formatters.SmallVecSynthProvider -x "^SmallVecImpl<.+>$"')
    debugger.HandleCommand('type synthetic add -l formatters.SmallVecSynthProvider -x "^SmallVec<.+>$"')
    debugger.HandleCommand('type synthetic add -l formatters.PointersIntsVariantSynthProvider -x "^PointersIntsVariant<.+>$"')
    debugger.HandleCommand('type summary add -F formatters.pointersintsvariant_summary -x "^PointersIntsVariant<.+>$" -e true')
    debugger.HandleCommand('type summary add -F formatters.bigint_summary -x "BigInt" -e true')
    debugger.HandleCommand('type summary add -F formatters.bigint_summary -x "ConstantRef" -e true')
    debugger.HandleCommand('type summary add -F formatters.bigint_summary -x "GenericBigIntRef" -e true')
    #debugger.HandleCommand('type synthetic add -l formatters.DenseMapSynthProvider -x "^DenseMap<.+>$"')
    #debugger.HandleCommand('type summary add -F formatters.smallvec_summary -x "^SmallVecImpl<.+>$"')
