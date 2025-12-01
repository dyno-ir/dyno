


import cocotb
from cocotb.triggers import Timer
import pprint

@cocotb.test()
async def my_first_test(dut):
    for i in range(10):
        dut.in_a.value = i
        await Timer(1, unit="ns")
        assert dut.out_a.value == i + 1
        attrs = {name: getattr(dut.out_a, name) for name in dir(dut.out_a) if not name.startswith("__")}
        pprint.pprint(attrs)

