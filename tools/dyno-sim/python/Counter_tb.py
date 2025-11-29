


import cocotb
from cocotb.triggers import Timer
import pprint
import random

@cocotb.test()
async def my_first_test(dut):
    dut.clk.value = 0
    dut.rst.value = 1
    dut.en.value = 0
    await Timer(2, unit="ns")
    for i in range(100):
        dut.clk.value = ~dut.clk.value
        if i == 10:
            dut.rst.value = 0
        if i == 14:
            dut.en.value = 1
        #if int(dut.clk.value) == 0:
        #    dut.en.value = random.randint(0, 1)
        await Timer(1, unit="ns")


