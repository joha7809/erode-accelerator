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
  val idle :: start :: borderCheck :: beginMow :: checkBlack :: incinerateMiddle :: incinerateLeft :: incinerateTop :: incinerateRight :: incinerateBottom :: mopUp :: write :: increment :: harvest :: idk :: end :: Nil =
    Enum(15);

  val stateReg = RegInit(idle)

  val x = RegInit(0.U(16.W))
  val y = RegInit(0.U(16.W))

  val pixelColor = RegInit(1.U(32.W))
  val writeColor = RegInit(0.U(32.W))
  val currentAddress = Reginit(0.U(32.W))

  val cleanStep = Reginit(0.U(32.W))
  val mopUpStep = Reginit(0.U(32.W))

  val topleftMop3 = Reginit(0.U(32.W))
  val bottomleftMop3 = Reginit(0.U(32.W))
  val topleftMop6 = Reginit(0.U(32.W))
  val bottomleftMop6 = Reginit(0.U(32.W))
  val topleftMop9 = Reginit(0.U(32.W))
  val bottomleftMop9 = Reginit(0.U(32.W))
  val topleftMop12 = Reginit(0.U(32.W))
  val bottomleftMop12 = Reginit(0.U(32.W))
  val topleftMop15 = Reginit(0.U(32.W))
  val bottomleftMop15 = Reginit(0.U(32.W))
  val topleftMop18 = Reginit(0.U(32.W))
  val bottomleftMop18 = Reginit(0.U(32.W))

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
        stateReg := beginMow
        x := 3.U(16.W)
        y := 3.U(16.W)

      }
    }

    //    is(borderCheck) {
    //      when(x === 0.U || x === 19.U || y === 0.U || y === 19.U) {
    //        writeColor := 0.U(32.W)
    //        pixelColor := 0.U(32.W)
    //        stateReg := write;
    //      }.otherwise {
    //        io.address := (x + y * 20.U)
    //        dataReg := io.dataRead
    //        stateReg := checkBlack
    //      }
    //    }

    is(beginMow) {
      currentAddress := x + y * 20.U
      stateReg := checkBlack
    }


    is(checkBlack) {
      when(dataReg === 0.U(32.W)) {
        dataReg := io.dataRead
        pixelColor := 0.U(32.W)
        stateReg := incinerateMiddle
      }//.otherwise {
//        io.address := (x + (y + 1.U) * 20.U) // set address to y below
//        dataReg := io.dataRead
//        stateReg := checkBelow
//      }
    }

    is(incinerateMiddle) {
      writeColor := 0.U;
      cleanStep := 1;
      stateReg := write;
    }

    is(incinerateLeft) {
      writeColor := 0.U;
      cleanStep := 2;
      stateReg := write;
    }

    is(incinerateTop){
      writeColor := 0.U;
      cleanStep := 3;
      stateReg := write;
    }

    is(incinerateRight){
      writeColor := 0.U;
      cleanStep := 4;
      stateReg := write;
    }

    is(incinerateBottom){
      writeColor := 0.U;
      cleanStep := 5;
      stateReg := write;
    }

    is(mopUp) {
      when(x === 3){

      }.elsewhen(x===6){

      }.elsewhen(x===9){

      }.elsewhen(x===12){

      }.elsewhen(x===15){

      }.elsewhen(x===18){

      }
    }



    is(write) {
      when(cleanStep === 1){
        io.address := (x + y * 20.U) + 400.U
        io.dataWrite := writeColor
        io.writeEnable := 1.B

        stateReg := incinerateLeft
      }.elsewhen(cleanStep === 2){
        io.address := (x - 1.U + y * 20.U) + 400.U
        io.dataWrite := writeColor
        io.writeEnable := 1.B

        stateReg := incinerateTop;

      }.elsewhen(cleanStep === 3){
        io.address := (x + 1.U + y * 20.U) + 400.U
        io.dataWrite := writeColor
        io.writeEnable := 1.B

        stateReg := incinerateRight;
        }.elsewhen(cleanStep === 4){
        io.address := (x + 1.U + y * 20.U) + 400.U
        io.dataWrite := writeColor
        io.writeEnable := 1.B

        stateReg := incinerateBottom;
      }.elsewhen(cleanStep === 5){
        io.address := (x + (y+1.U) * 20.U) + 400.U
        io.dataWrite := writeColor
        io.writeEnable := 1.B
        cleanStep := 0
        stateReg := increment;
      }


      stateReg := increment1
    }

    is(increment) {
      when(x === 18.U && y === 18.U){
        stateReg := harvest
      }
      .elsewhen(x === 18.U) {
        stateReg := mopUp
      }

      x := x + 3.U(16.W)
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

