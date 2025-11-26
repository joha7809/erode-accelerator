import chisel3._
import chisel3.util._

class Accelerator extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val done = Output(Bool())

    val address = Output(UInt (16.W))
    val dataRead = Input(UInt (32.W))
    val writeEnable = Output(Bool ())
    val dataWrite = Output(UInt (32.W))

  })

  //State enum and register
  val idle :: start :: borderCheck :: checkBlack :: checkBelow 
            :: checkAbove :: checkLeft :: checkRight :: writeBlack 
            :: writeWhite :: write :: increment1 :: increment2 
            :: end :: Nil = Enum (15);
  val stateReg = RegInit(idle)
  val x = RegInit(0.U(16.W))
  val y = RegInit(0.U(16.W))
  val pixelColor = RegInit(1.U(32.W))
  val writeColor = RegInit(0.U(32.W))
  val writtenTwice = RegInit(0.B)
  
  
  //FSMD switch
  switch(stateReg) {
    is(idle) {
      when(io.start) {
        stateReg := borderCheck
        x := 0.U(16.W)
        y := 0.U(16.W)
      }
    }
    
    is(borderCheck) {
      if (x == 0.U || x == 19.U || y == 0.U || y == 19.U) {
        writeColor := 0.U(32.W)
        pixelColor := 0.U(32.W)
        stateReg := write;
      } else {
        stateReg := checkBlack
      }
    }

    is(checkBlack) {
      if io.dataRead(io.address(x+y*20.U))==0.U(32.W) {
        pixelColor := 0.U(32.W)
        stateReg := writeBlack
      } else {
        stateReg := checkBelow
      }
    }

    is(checkBelow) {
      if io.dataRead(io.address(x+(y+1.U)*20.U))==0.U(32.W) {
        pixelColor := 0.U(32.W)
        stateReg := writeBlack
      } else {
        stateReg := checkAbove
      }
    }

    is(checkAbove) {
      if io.dataRead(io.address(x+(y-1.U)*20.U))==0.U(32.W) {
        pixelColor := 0.U(32.W)
        stateReg := writeBlack
      } else {
        stateReg := checkLeft
      }
    }

    is(checkLeft) {
      if io.dataRead(io.address((x-1.U)+y*20.U))==0.U(32.W) {
        pixelColor := 0.U(32.W)
        stateReg := writeBlack
      } else {
        stateReg := checkRight
      }
    }

    is (checkRight) {
      if io.dataRead(io.address((x+1.U)+y*20.U))==0.U(32.W) {
        pixelColor := 0.U(32.W)
        stateReg := writeBlack
      } else {
        stateReg := writeWhite
      }
    }
    
    is (writeBlack) {
        writeColor := 0.U(32.W)
        stateReg := write
    }
    
    is (writeWhite) {
        writeColor := 1.U(32.W)
        stateReg := write
    }

    is (write) {
      io.address := x + y * 20.U
      io.dataWrite := writeColor
      io.writeEnable := 1.B
      stateReg := increment1
    }

    is (increment1) {
      x = x + 1.U(16.W)
      if x == 19 {
        stateReg := increment2
      } else if pixelColor == 0 && writtenTwice == 0 && x != 0 && x != 19 {
        writtenTwice := 1.B
        stateReg := write
      } else if x != 19 {
        stateReg := borderCheck
        writtenTwice := 0.B
        pixelColor := 1.U(32.W)
      }
    }

    is (increment2) {
      x = 0.U(16.W)
      y = y + 1.U(16.W)
      if y == 20.U {
        stateReg := end
      } else {
        stateReg := borderCheck
        writtenTwice := 0.B
        pixelColor := 1.U(32.W)
      }
    }

  }
}