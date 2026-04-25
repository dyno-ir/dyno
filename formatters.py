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


def __lldb_init_module(debugger, internal_dict):
    print("loading SmallVec vis function")
    debugger.HandleCommand('type synthetic add -l formatters.SmallVecSynthProvider -x "^SmallVecImpl<.+>$"')
    debugger.HandleCommand('type synthetic add -l formatters.SmallVecSynthProvider -x "^SmallVec<.+>$"')
    #debugger.HandleCommand('type synthetic add -l formatters.DenseMapSynthProvider -x "^DenseMap<.+>$"')
    #debugger.HandleCommand('type summary add -F formatters.smallvec_summary -x "^SmallVecImpl<.+>$"')
