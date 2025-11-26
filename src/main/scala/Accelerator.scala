import chisel3._
import chisel3.util._

class Accelerator extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val done = Output(Bool())

    val address = Output(UInt(16.W))
    val dataRead = Input(UInt(32.W))
    val writeEnable = Output(Bool())
    val dataWrite = Output(UInt(32.W))

  })

  // State enum and register
  val idle :: start :: borderCheck :: checkBlack :: checkBelow :: checkAbove :: checkLeft :: checkRight :: writeBlack :: writeWhite :: write :: increment1 :: increment2 :: end :: Nil =
    Enum(14);
  val stateReg = RegInit(idle)
  val x = RegInit(0.U(16.W))
  val y = RegInit(0.U(16.W))
  val pixelColor = RegInit(1.U(32.W))
  val writeColor = RegInit(0.U(32.W))
  val writtenTwice = RegInit(0.B)

  val dataReg = RegInit(0.U(32.W))

  // Default values
  io.writeEnable := false.B
  io.done := false.B
  io.dataWrite := false.B
  io.address := 0.U

  // FSMD switch
  switch(stateReg) {
    is(idle) {
      when(io.start) {
        stateReg := borderCheck
        x := 0.U(16.W)
        y := 0.U(16.W)
      }
    }

    is(borderCheck) {
      when(x === 0.U || x === 19.U || y === 0.U || y === 19.U) {
        writeColor := 0.U(32.W)
        pixelColor := 0.U(32.W)
        stateReg := write;
      }.otherwise {
        io.address := (x + y * 20.U)
        dataReg := io.dataRead
        stateReg := checkBlack
      }
    }

    is(checkBlack) {
      when(dataReg === 0.U(32.W)) {
        pixelColor := 0.U(32.W)
        stateReg := writeBlack
      }.otherwise {
        io.address := (x + (y + 1.U) * 20.U) // set address to y below
        dataReg := io.dataRead
        stateReg := checkBelow
      }
    }

    is(checkBelow) {
      when(
        io.dataRead === 0.U(32.W)
      ) {
        stateReg := writeBlack
      }.otherwise {
        io.address := (x + (y - 1.U) * 20.U) // set address to y above
        dataReg := io.dataRead
        stateReg := checkAbove
      }
    }

    is(checkAbove) {
      when(
        io.dataRead === 0.U(32.W)
      ) {
        stateReg := writeBlack
      }.otherwise {
        io.address := ((x - 1.U) + y * 20.U) // set address to x left
        dataReg := io.dataRead
        stateReg := checkLeft
      }
    }

    is(checkLeft) {
      when(
        io.dataRead === 0.U(32.W)
      ) {
        stateReg := writeBlack
      }.otherwise {
        io.address := ((x + 1.U) + y * 20.U) // set address to x right
        dataReg := io.dataRead
        stateReg := checkRight
      }
    }

    is(checkRight) {
      when(
        io.dataRead === 0.U(32.W)
      ) {
        stateReg := writeBlack
      }.otherwise {
        pixelColor := 1.U(32.W)
        stateReg := writeWhite
      }
    }

    is(writeBlack) {
      writeColor := 0.U(32.W)
      stateReg := write
    }

    is(writeWhite) {
      writeColor := 255.U(32.W)
      stateReg := write
    }

    is(write) {
      io.address := x + y * 20.U
      io.dataWrite := writeColor
      io.writeEnable := 1.B
      stateReg := increment1
    }

    is(increment1) {
      x := x + 1.U(16.W)
      when(x === 19.U) {
        stateReg := increment2
      }.elsewhen(
        (pixelColor === 0.U) &&
          (writtenTwice === 0.U) &&
          (x =/= 0.U) &&
          (x =/= 19.U)
      ) {
        writtenTwice := 1.B
        stateReg := write
      }.elsewhen(x =/= 19.U) {
        stateReg := borderCheck
        writtenTwice := 0.B
        pixelColor := 1.U(32.W)
      }
    }

    is(increment2) {
      x := 0.U(16.W)
      y := y + 1.U(16.W)
      when(y === 20.U) {
        stateReg := end
      }.elsewhen(y =/= 20.U) {
        stateReg := borderCheck
        writtenTwice := 0.B
        pixelColor := 1.U(32.W)
      }
    }

    is(end) {
      io.done := true.B
    }

  }
}

