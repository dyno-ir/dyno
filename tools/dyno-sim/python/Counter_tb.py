


import cocotb
from cocotb.triggers import Timer
import pprint
import random

@cocotb.test()
async def my_first_test(dut):
    for i in range(100):
        dut.en.value = random.randint(0, 1)
        dut.clk.value = ~dut.clk.value
        await Timer(1, unit="ns")


