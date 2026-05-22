package examples

import vutils._
import vutils.datatype._
import chisel3._

class Float32MAC extends Module {
  val io = IO(new Bundle {
    val in0 = Input(Float(32.W, 23.BP))
    val in1 = Input(Float(32.W, 23.BP))
    val in2 = Input(Float(32.W, 23.BP))
    val out = Output(Float(32.W, 23.BP))
  })

  io.out := (io.in0 + io.in1) * io.in2
}

object Float32MACExample extends App {
  DesignEmitter.emit(
    gen = new Float32MAC,
    filename = "float32_mac",
    target = SystemVerilog,
    info = true,
    lowering = true,
  )
}
