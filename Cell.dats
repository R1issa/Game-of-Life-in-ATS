#define ATS_MAINATSFLAG 1
#define ATS_DYNLOADNAME "theClicks2_baconjs_start"
#define ATS_STATIC_PREFIX "theClicks2_baconjs_"

#include "share/atspre_define.hats"
#include "{$LIBATSCC2JS}/staloadall.hats"

staload "{$LIBATSCC2JS}/SATS/print.sats"
staload _ = "{$LIBATSCC2JS}/DATS/print.dats"
staload "{$LIBATSCC2JS}/SATS/Bacon.js/baconjs.sats"

%{^
//
var theCounter = 0
var theCounter2 = 0
//
%}

extern
fun updateAllGrid(): void = "mac#"
extern
fun random_grid(i:int, j: int): void = "mac#"
extern
fun reset_grid(i:int, j: int): void = "mac#"
extern
fun applyTemplate(name: string, len_x: int, len_y: int, pos_x: int, pos_y: int): void = "mac#"
extern
fun logic_build(): void = "mac#"


// Some basic functions/////
fun dbl2int(d: double): int =
  if d < 0.5 then 0
  else 1

fun int2bool(i: int): bool =
  if i=1 then true
  else false
/////////////////////////////

datatype act = Click of (int,int)
datatype simClick = Sim | Stop | Skip | Random | Logic
datatype config = Reset

val size = $extval(int, "size")
// This Allows the User to Make one step in the simulation
val simulateClicks = $extfcall(EStream(void), "getStreamForId", "simOnce")
val simulateClicks = map(simulateClicks, lam(x) =<cloref1> Sim())

// This Stops the simulation
val simStop = $extfcall(EStream(void), "getStreamForId", "simlStop")
val simStop = map(simStop, lam(x) =<cloref1> Stop())

// If this button is pressed and then the SimulateOnce button is pressed
// then the Simulation runs for indefinetly many steps until the user
// presses the Stop! button
val simulateAll = $extfcall(EStream(void), "getStreamForId", "simAll")
val simulateAll = simulateAll.map(TYPE{simClick})(lam _ => Skip())

// This Allows the User to Make one step in the simulation
val logic = $extfcall(EStream(void), "getStreamForId", "logic")
val logic = map(logic, lam(x) =<cloref1> Logic())

// This Allows the User to Make one step in the simulation
val random = $extfcall(EStream(void), "getStreamForId", "randConfig")
val random = map(random, lam(x) =<cloref1> Random())

// This Allows the User to Make one step in the simulation
val reset = $extfcall(EStream(void), "getStreamForId", "resetConfig")
val reset = map(reset, lam(x) =<cloref1> Reset())

// Maping stream of click to each cell in order to either kill
// or make a cell alive at any point before/during the simulation
fun initStreams(i:int, j: int): void =
  if i = size then ()
  else if j = size then initStreams(i+1, 0)
  else
    let
      val si = String(i)
      val sj = String(j)

      // cellClicks allows the user to Kill or make a cell alive
      val cellClicks = $extfcall(EStream(void), "getStream", i, j)
      val cellClicks = map(cellClicks, lam(x) =<cloref1> Click(i, j))

      // Handling every stream of clicks seperatly in order to avoid
      // too much recursion due to "merge"

      // If a cell is clicked, flip its status
      // If dead -> becomes alive
      // If alive -> becomes dead
      val cellClicks = scan{int}{act}(cellClicks, 0, lam(res, click) =>
            let
              val Click(i: int, j: int) = click
              val template_name = $extfcall(string, "getSelectedTemplate")
            in
              if template_name = "default" || template_name = "" then res + 1 where { val () = $extfcall(void, "flipCell", i, j) }
              else
                let
                  val x: int = $extfcall(int, "getTemplateRowCount", template_name)
                  val y: int = $extfcall(int, "getTemplateColCount", template_name)
                  val () = applyTemplate(template_name, x, y, i, j)
                in
                  res + 1
                end
            end )

      val () = $extmcall(void, $extval(ptr, "streams"), "push", cellClicks)
    in
      initStreams(i, j+1)
    end

// Initialize the stream of clicks
val allClicks = initStreams(0, 0)

///////////////////////////////////////////Creating Simulation and Stop buttons////////////////////////////////////////////////
////////////////////////////////////////////
// This is highly inspired from assignment 8
////////////////////////////////////////////

val simulateAll_toggles = scan{bool}{simClick}(merge(simulateAll, simStop), false , lam(res, simClick) =>
case+ simClick of
  |Stop() => false
  | _ => true
)
val theSims = simulateClicks
val theSims = merge(theSims, simStop)
val theSims = merge(theSims, random)
val theSims = merge(theSims, logic)

val SimComb_stream = Property_sampledBy_estream_cfun (simulateAll_toggles, theSims, lam(x, y) => if x then Skip else y)

// TIME SPEED
/// CHANGE TIME HERE IN ORDER TO SPEED UP OR SLOWDOWN SIMULATION
val SimTick_stream = Property_sampledBy_estream(simulateAll_toggles, Bacon_interval{int}(5(*ms*), 0))

val theSims = merge(theSims, simulateAll)
val theComb2_property = EStream_toProperty_init(theSims, Skip)

val theComb2Tick_stream = Property_sampledBy_estream_cfun(theComb2_property, SimTick_stream, lam(x, y) => if y then x else Skip)

val theCounter  = scan{int}{simClick} (merge(SimComb_stream, theComb2Tick_stream), 0, lam(state, click)=<cloref1>
    case+ click of
    | Sim() => let
                val () = updateAllGrid()
                val () = $extfcall(void, "update_steps", 0) // JUst show the number of iterations
               in
                state + 1
               end
    | Stop() => state
    | Skip() => state // Skip is acting as SimulateAll
    | Random() => let val () = random_grid(0, 0) in state end
    | Logic () => let val () = logic_build() in state end
)

val theCounter2  = scan{int}{config} ( reset, 0, lam(state, click)=<cloref1>
    case+ click of
    | Reset() => let
                  val() = reset_grid(0, 0)
                  val() = $extfcall(void, "update_steps", 1)
                 in
                  0
                 end
)
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
extvar "theCounter" = theCounter
extvar "theCounter2" = theCounter2

///////////////////////////////////////Updating Grid According to Game of Life Rules///////////////////////////////////////////

// In the case where we are trying to update a neighbhor
// who's index is out of bound,
// If the user has the "Torus Grid option" checked then
// the bottom row is linked to the top row, and the left row to the right row thus forming a sort of toroidal grid.
// If the user doesnt have the "Torus Grid option" checked then
// We will consider out of bound cells as dead and apply the rules accordingly
fun fix_index(index: int): int =
  let
    val t = $extfcall(bool, "mycheckbox")
  in
    if t then
      if index >= size then 0
      else if index < 0 then size -1
           else index
    else // if the user doesn't want to consider the grid as a
        // torus then return -1 and let getCell_copy handle it
       // in this case treat the outofbound edges as dead
      if index >= size then ~1
      else if index < 0 then ~1
          else index
  end

// Update a cell based on its surrounding neighbhors
fun updateGrid(i: int, j: int): void =
  let

    // Get the cells status
    val current_state = $extfcall(int, "getCell_copy", i, j)

    // First get the status of a cells neighbhors
    val left_nghbr = $extfcall(int, "getCell_copy", fix_index(i), fix_index(j-1))
    val right_nghbr = $extfcall(int, "getCell_copy", fix_index(i), fix_index(j+1))

    val up_nghbr = $extfcall(int, "getCell_copy", fix_index(i-1), fix_index(j))
    val down_nghbr = $extfcall(int, "getCell_copy", fix_index(i+1), fix_index(j))

    val uleft_nghbr = $extfcall(int, "getCell_copy", fix_index(i-1), fix_index(j-1))
    val uright_nghbr = $extfcall(int, "getCell_copy", fix_index(i-1), fix_index(j+1))

    val dleft_nghbr = $extfcall(int, "getCell_copy", fix_index(i+1), fix_index(j-1))
    val dright_nghbr = $extfcall(int, "getCell_copy", fix_index(i+1), fix_index(j+1))

    // Count number of living cells
    val live_nghbr = left_nghbr + right_nghbr + up_nghbr + down_nghbr
    val live_nghbr = live_nghbr + uleft_nghbr + uright_nghbr + dleft_nghbr + dright_nghbr

  in // If a cell is alive and has 2 living neighbhors, or if a cell has 3 living neighbhors, then it must live
    if (live_nghbr = 2 && current_state = 1) || live_nghbr = 3 then $extfcall(void, "setCell", i, j, true)
    else $extfcall(void, "setCell", i, j, false) // otherwise die
  end

// Update all the grid by looping over all i's and j's
implement updateAllGrid() =
  let
    val () = $extfcall(void, "updateCopy")
    fun aux(i: int, j: int): void =
      if i >= size then ()
      else
        if j >= size then aux(i+1, 0)
        else
          let
            // Note that the original grid is immediately updateGrid
            // and the copy of the grid is the one where we check if
            // the rules hold or not, in order to update
            val () = updateGrid(i, j)
          in
            aux(i, j+1)
          end
  in
    aux(0, 0)
  end

// This method allows the user to place the template anywhere he likes
// In the case of a toroidal grid, the template loopsback to the start or
// the end of the row/columns.
// In the case of a non toroidal grid, consider any out of bound index as
// dead
fun fix_index_template(index: int): int =
    let
      val t = $extfcall(bool, "mycheckbox")
    in
      if t then
        if index >= size then index - size
        else if index < 0 then size + index
             else index
      else // if the user doesn't want to consider the grid as a
          // torus then return -1 and let getCell_copy handle it
         // in this case treat the outofbound edges as dead
        if index >= size then ~1
        else if index < 0 then ~1
            else index
    end


// Add Template to grid according to where user clicked.
// The click position will be top most left most cell of the
// template
implement applyTemplate(name, len_x, len_y, pos_x, pos_y) =
  let
    fun aux(i: int, j: int): void =
      if i = len_x then ()
      else if j = len_y then aux(i+1, 0)
      else
        let
          val status = $extfcall(bool, "getTemplateAt", name, i, j)
          val () = $extfcall(void, "setCell", fix_index_template(pos_x + i), fix_index_template(pos_y + j), status)
        in
          aux(i, j+1)
        end
  in
    aux(0, 0)
  end

implement random_grid(i, j) = // generate a random grid
  if i = size then ()
  else if j = size then random_grid(i+1, 0)
  else
    let
      val stat = dbl2int(JSmath_random())
      val () = $extfcall(void, "setCell", i, j, int2bool(stat))
      val () = $extfcall(void, "updateCopy")
    in
     random_grid(i, j+1)
    end

implement reset_grid(i, j) = // kill all grid cells
  if i = size then $extfcall(void, "updateCopy")
  else if j = size then reset_grid(i+1, 0)
  else
    let
      val () = $extfcall(void, "setCell", i, j, false)
    in
      reset_grid(i, j+1)
    end

fun build_T(pos_x: int, pos_y: int): void =
let
  val x: int = $extfcall(int, "getTemplateRowCount", "glider_gun")
  val y: int = $extfcall(int, "getTemplateColCount", "glider_gun")
  val () =   applyTemplate("glider_gun", x, y, pos_x, pos_y)
in
end
//11
//24
fun build_F(pos_x: int, pos_y: int): void =
let
  val x: int = $extfcall(int, "getTemplateRowCount", "glider_gun")
  val y: int = $extfcall(int, "getTemplateColCount", "glider_gun")
  val x2: int = $extfcall(int, "getTemplateRowCount", "blocker")
  val y2: int = $extfcall(int, "getTemplateColCount", "blocker")
  val () =   applyTemplate("glider_gun", x, y, pos_x, pos_y)
  val () =   applyTemplate("blocker", x2, y2, pos_x + 10, pos_y + 24)
in
end

fun check_val (p: int, pos_x: int, pos_y: int): void =
  if p = 1 then build_T(pos_x, pos_y)
  else build_F(pos_x, pos_y)

fun signalr(pos_x: int, pos_y: int): void =
let
  val x: int = $extfcall(int, "getTemplateRowCount", "glider_gunr")
  val y: int = $extfcall(int, "getTemplateColCount", "glider_gunr")
  val () =   applyTemplate("glider_gunr", x, y, pos_x, pos_y)
  val () =   applyTemplate("glider_gunr", x, y, pos_x, pos_y)
in
end

fun signal(pos_x: int, pos_y: int): void =
let
  val x: int = $extfcall(int, "getTemplateRowCount", "glider_gun")
  val y: int = $extfcall(int, "getTemplateColCount", "glider_gun")
  val () =   applyTemplate("glider_gun", x, y, pos_x, pos_y)
  val () =   applyTemplate("glider_gun", x, y, pos_x, pos_y)
in
end


fun build_and(): void =
let
  val a = $extfcall(int, "get_logicExp_at_i", 0)
  val b = $extfcall(int, "get_logicExp_at_i", 1)
  val () = check_val(a, 0, 0)
  val () = check_val(b, 0, 42)
  val () = signalr(0, 82)
in
end

fun build_not(): void =
let
  val a = $extfcall(int, "get_logicExp_at_i", 0)
  val () = check_val(a, 0, 0)
  val () = signalr(0, 42)
in
end

fun build_or(): void =
let
  val a = $extfcall(int, "get_logicExp_at_i", 0)
  val b = $extfcall(int, "get_logicExp_at_i", 1)
  val () = signal(0, 0)
  val () = check_val(a, 2, 42)
  val () = check_val(b, 1, 76)
  val () = signalr(0, 120)
in
end


implement logic_build() =
  let
    val typ = $extfcall(int, "getExpType")
  in
   if typ = 1 then build_and()
   else if typ = 2 then build_or()
        else build_not()
  end
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


%{$
var size = 0;
var time = 50;
var torus = false;
var steps = 0;
var colors = ['#bf1fcd','#792983','#f58706'];
var grid = [];
var grid_copy = [];
var expression = [];

var streams = []; //where each entry is a stream of clicks per cell


///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

//Check if user wants a toroidal grid
function mycheckbox() {
    return document.getElementById("torus").checked;
}
// Count the number of simulation iterations
function update_steps(reset){
  if(reset == 1){
    steps = 0;
  }else{
    steps = steps + 1;
  }
  $("#counter").text(steps);
}
function rand(){
  return ~~(Math.random()*2) ? true : false;
}
// randomly set a cells color whenever the grid is updated
function rand_color(){
  return colors[Math.floor(Math.random()*3)];
}

// Get the click event for a cell[i][j]
function getStream(i, j) {
  return $("#"+"c"+i+"_"+j).asEventStream("click");
}

// Get a stream per cell
function getStreamForId(str_id){
  return $("#"+str_id).asEventStream("click");
}

// Set a Cell to alive or dead
function setCell(i, j, life) {

  var status = "dead";
  var color = '#f2f2ef';
  if(life){
   status = "alive";
   color = rand_color();
  }
  grid[i][j] = life;
  document.getElementById("c"+i+"_"+j).className = "gridsquare " + status;
  document.getElementById("c"+i+"_"+j).style.backgroundColor = color;
}
// Note that the original grid is updated immediately
// and the copy is the one checked in order to update
function getCell_copy(i, j) {
  if(i == -1 || j == -1){ // consider an out of bound cell dead
    return 0;
  }
  if(grid_copy[i][j]){
    return 1;
  }
  return 0;
}
// Set the grid_copy to grid once the updating is done
function updateCopy() {
  for (var i = 0; i < size; i++) {
    for (var j = 0; j < size; j++) {
      grid_copy[i][j]= grid[i][j];
    }
  }
}

// Flip a cell's status if its clicked
function flipCell(i, j) {
  grid[i][j] = !grid[i][j];

  var color = '#f2f2ef';
  var status = "dead";

  if(grid[i][j]){
    status = "alive";
    color = '#204eee';
  }

  document.getElementById("c"+i+"_"+j).className = "gridsquare " + status;
  document.getElementById("c"+i+"_"+j).style.backgroundColor = color;
}

// Generate a table of size n
function genTable(n) {
    size = n;

    grid = new Array(n);
    grid_copy = new Array(n);

    var width = 28 * n;
    var e = document.getElementById("sim")
    for (var i = 0; i < n; i++) {
        grid[i] = new Array(n);
        grid_copy[i] = new Array(n);

        var row = document.createElement("div");
        row.className = "row";
        row.style.width = "" + width + "px";

        for (var j = 0; j < n; j++) {

            var cell = document.createElement("div");
            cell.className = "gridsquare dead";
            cell.innerHTML = "&nbsp;"
            cell.id = "c"+i+"_"+j;
            row.appendChild(cell);
            grid[i][j] = false;
        }
        e.appendChild(row);
    }
}
function Start(){

  var n = parseInt(document.getElementById("size_n").value);
  genTable(n);

  var _ = theClicks2_baconjs_start();
  for(var i = 0; i < streams.length; i++) { // assign each stream of clicks
      var _ = streams[i].assign($("#ass"), "text");
  }

  var _ = theCounter.assign($("#siass"), "text");
  var _ = theCounter2.assign($("#siass2"), "text");
  $("#gridcontrol").hide();
}
// check if a templates[x][y] i.e. at its cell at its x'th row and y'th col
// is dead or alive.
function getTemplateAt(templateName, x, y) {
  console.log(templateName);
  console.log(x);
  console.log(y);
  return templates[templateName][x][y];
}
// Set the Default option
function resetTemplate() {
  $("#templates").val("default");
}
// Return the user selected Template
function getSelectedTemplate() {
  return $("#templates").val();
}
// Count the number of rows in the template
function getTemplateRowCount(name) {
  return templates[name].length;
}
// Count the number of columns in the template
function getTemplateColCount(name) {
  return templates[name][0].length;
}
function getExpType(){
  var t = $("#exp").val()
  if (t == "and"){
    return 1;
  }else if (t == "or"){
    return 2;
  }
  return 3;
}

function get_logicExp_at_i(i){
  if(expression == ""){
   expression = document.getElementById("expression").value;
  }
  if(expression.charAt(i) == "T"){
    return 1;
  }
  return 0;
}

%}
