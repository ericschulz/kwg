////////////////////////////////////////////////////////////////////////
//            JS-CODE FOR 2D ACTIVE FUNCTION LEARNING                 //
//        cordova version and refactoring: Federico Meini             //
//            original work: CHARLEY WU, ERIC SCHULZ                  //
////////////////////////////////////////////////////////////////////////

// Touch events support
var clickEventType = "click";
window.addEventListener('touchstart', function() {
  clickEventType = "touchstart";
});

//EXPERIMENT PARAMETERS
var fullurl = document.location.href, //url of incoming MTurk worker
  totalTrialsNumber = 10, // includes the instructions trial and the bonus trial
  trials = totalTrialsNumber, //number of REMAINING trials
  trialCounter = -1, //counter for current trial number
  horizon = 25,
  tracker = new Array(0), //tracker array
  investigationIndex = 0, //current click number
  scoretotal = [],
  scorecurrent = 0,
  reward = 0.00,
  starArray = [],
  gridMax = [],
  envOrder = getRandomSubarray([0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39], totalTrialsNumber),
  currentEnv = envOrder[0],
  environmentList = [],
  scale = [],
  //Color parameters for heatmap
  colors = ['#fff7ec', '#fee8c8', '#fdd49e', '#fdbb84', '#fc8d59', '#ef6548', '#d7301f', '#b30000', '#7f0000'],
  heatmapColor = d3.scale.linear().domain(d3.range(0, 50, 50.0 / (colors.length - 1))).range(colors),
  testerNotes = {};

//data collectors for search history
var tscollect = [],
  xcollect = [],
  ycollect = [],
  zcollect = [],
  zcollectScaled = [],
  initcollect = [],
  bonusCollect = {"bonusCells":[], finalChosenCell:null};

/*
- x,y of the 5 randomly selected tiles
- mean judgements (slider value): 0 - 50
- confidence (radios) 1 - 7
- index of chosen tile among the five (and this last tile gets also played and added to xcollect, ycollect....)
 */

//Populate data collectors with empty arrays
//Todo: change max clicks to the assigned horizon for each trial
for (var i = 0; i < totalTrialsNumber; i++) {
  scale[i] = randomNum(30, 40);
  scoretotal[i] = 0;
  starArray[i] = 0;
  gridMax[i] = 0;
  tscollect[i] = [];
  xcollect[i] = [];
  ycollect[i] = [];
  zcollect[i] = [];
  zcollectScaled[i] = [];
  initcollect[i] = [];
}

// Set initial counters
if (localStorage.getItem("gridsearch-counter-1") === null)
  localStorage.setItem("gridsearch-counter-1", 0);

if (localStorage.getItem("gridsearch-counter-2") === null)
  localStorage.setItem("gridsearch-counter-2", 0);

if (localStorage.getItem("gridsearch-counter-3") === null)
  localStorage.setItem("gridsearch-counter-3", 0);


//Declare variables not yet assigned 
var condition,
  gender,
  age = 0,
  birthDate,
  grade,
  searchHistory,
  initialEnvs,
  changeEnvs,
  roundScore,
  xout,
  yout,
  zout,
  optimaGuess;



//Access the MySQL database and returns scenario id with a condition numnber, adding the now() date time to the start time field and marking the specific scenario as completed
function assignScenario() {
  //Extract gender and birthdate
  if (document.getElementById('Male').checked) {
    gender = "Male";
  }
  if (document.getElementById('Female').checked) {
    gender = "Female";
  }

  var dob = document.getElementById('birthDate').value;
  grade = parseInt(document.getElementById('gradeSelect').value);


  //Check if any data missing
  var checksum = (typeof (gender) !== 'undefined') && dob != null && dob != "";
  if (checksum) {

    age = getAge(dob);

    var counter = 0;
    if (age <= 9) {
      counter = getCounter("gridsearch-counter-1");
    } else if (age > 9 && age < 18) {
      counter = getCounter("gridsearch-counter-2");
    } else if (age > 18) {
      counter = getCounter("gridsearch-counter-3");
    }

    condition = counter % 2;
    //condition=Math.round(Math.random())
    clicks = horizon; //set initial number of clicks to horizon
    //var kernelFiles = ['kernelSmooth.json', 'kernelRough.json', 'kernelMed.json'];
    //Load initial environments
    if (condition == 0) { //initial env is smooth
      var initialEnvs = smoothKernel; // variable from json file
    } else {
      var initialEnvs = roughKernel; // variable from json file
    }
    //changeEnvs = medKernel;
    //put environments from the randomized envOrder into environmentList
    for (i = 0; i <= trials; i++) {
      environmentList[envOrder[i]] = initialEnvs[envOrder[i]];
    }
    //Advance the page
    nexttrial();
    clickStart('page1', 'page5');
  } else {
    alert("Fehlende Daten.") // remind to fill out all the data
  }

  setButtonHandlers();

}


//Checkers:
//initialize environments
var init = [];

function instructioncheck() {
  //check if correct answers are provided
  if (document.getElementById('q1b').checked) {
    var ch1 = 1
  }
  if (document.getElementById('q2c').checked) {
    var ch2 = 1
  }
  if (document.getElementById('q3d').checked) {
    var ch3 = 1
  }
  //are all of the correct
  var checksum = ch1 + ch2 + ch3;
  if (checksum === 3) {
    //start game if instructions are checked correct
    clicks = 0;
    gridDeactivated = false;
    nexttrial(); 
    clickStart('page3', 'page5');
  } 
  else {
    //if one or more answers are wrong, raise alert box
    alert('Du hast einige Fragen falsch beantwortet. Bitte versuche es erneut');

    // And reset instructions and interactive demo
    trials = totalTrialsNumber - 1;
    instructionsCounter = 0;
    //gridDeactivated = true;
    document.getElementById("sidebarContent").style.display = "none";
    document.getElementById("sidebarInstructions").style.display = "block";
    clickStart('page3', 'page5');
  }
}



/**
 * onDocumentReady
 */
document.addEventListener("DOMContentLoaded", function (event) {
  document.getElementById("buttonAssignScenario").addEventListener("click", function () {
    assignScenario();
  });
});


/**
 * Sets the button handlers
 */
function setButtonHandlers() {
  document.getElementById("goToPage7").addEventListener(clickEventType, function () {
    clickStart('page6', 'page7');
  });
  document.getElementById("finishButton").addEventListener(clickEventType, function () {
    //senddata();
    onButtonFinishPressed();
  });
  document.getElementById("nextTrialButton").addEventListener(clickEventType, function () {
    nexttrial();
  });
  document.getElementById("buttonGoToPageFive").addEventListener(clickEventType, function () {
    clickStart('page4', 'page5');
  });
  document.getElementById("buttonInstructionsCheck").addEventListener(clickEventType, function () {
    instructioncheck();
  });
  document.getElementById("buttonGoToPageThree").addEventListener(clickEventType, function () {
    clickStart('page2', 'page3');
  });
  document.getElementById("buttonGoToBonusLevel").addEventListener(clickEventType, function () {
    nextBonusLevelStep();
  });
  document.getElementById("buttonNextBonus").addEventListener(clickEventType, function () {
    saveBonusStep();
  });
  document.getElementById("buttonInstructions").addEventListener(clickEventType, function () {
    onButtonInstructionsPressed();
  });
}


/**
 * Creates the grid in the DOM
 */
function createGrid() {
  var i, j, WIDTH = 8,
    HEIGHT = 8,
    lastRevealedCellId;

  var table = document.createElement("table");
  table.setAttribute("id", "grid");
  table.setAttribute("class", "grid");

  for (var y = 0; y < HEIGHT; y++) {
    var tr = document.createElement("tr");
    for (x = 0; x < WIDTH; x++) {
      var td = document.createElement("td");
      //td.setAttribute("id", y + "x" + x);
      td.setAttribute("data-x", x);
      td.setAttribute("data-y", y);
      td.addEventListener(clickEventType, onCellTappedHandler);
      tr.appendChild(td);
    }
    table.appendChild(tr);

    var gridContainer = document.getElementById("gridDiv");
    gridContainer.innerHTML = "";
    gridContainer.appendChild(table);
  }
}




/**
 *  onCellTappedHandler()
 * @param {*} evt 
 */
function onCellTappedHandler(evt) {

  // get tapped Cell instance
  var x = evt.target.getAttribute("data-x");
  var y = evt.target.getAttribute("data-y");
  var cell = Cell.getCell(x, y);

  if (playingBonusRound) return onBonusCellTapped(cell);
  return onCellTapped(cell);

}


// Instructions status
var instructionsCounter = 0;
var gridDeactivated = true;

/**
 *  button Instructions event handler
 * @param {*} evt 
 */
function onButtonInstructionsPressed(evt) {

  if (instructionsCounter == 0) {
    // go to insturctions 2nd page
    document.getElementById("sidebarInstructions-1").style.display = "none";
    document.getElementById("sidebarInstructions-2").style.display = "block";
    if (clicks > 0) gridDeactivated = false;
    instructionsCounter++;
  }

  else if (instructionsCounter == 1) {
    if (clicks > 0) {
      alert("Bitte benutze alle 25 Klicks um das Spielfeld zu erkunden.");
      return;
    }
    gridDeactivated = true;
    document.getElementById("sidebarInstructions-2").style.display = "none";
    document.getElementById("sidebarInstructions-3").style.display = "block";
    instructionsCounter++;
  }

  else if (instructionsCounter == 2) {
    clickStart('page5', 'page3');
    document.getElementById("sidebarInstructions").style.display = "none";
    document.getElementById("sidebarInstructions-3").style.display = "none";
    document.getElementById("sidebarInstructions-1").style.display = "block";
    document.getElementById("sidebarContent").style.display = "block";
  }
}


function onButtonFinishPressed() {

  var optionA = document.querySelector('input[name="option-a"]:checked');
  if (optionA == null) {
    return alert("Fill option A!");
  }

  var notes = document.getElementById("tester-notes").value;

  testerNotes = {
    "option-a": parseInt(optionA.value),
    "notes": notes
  };

  senddata();
}


/**
 *  onCellTapped()
 * @param {Cell} cell 
 */
function onCellTapped(cell) {

  if (cell == null || gridDeactivated) return;

  cell.clicked();

  // update counters
  currentEnvNum = envOrder[trialCounter];
  investigationIndex = investigationIndex + 1;
  tracker[investigationIndex] = cell.x + "x" + cell.y;

  //update number of clicks left
  clicks = clicks - 1;
  change("remaining2", "Verbleibende Klicks: <b>" + clicks + "</b>");

  //Update maximum reward found
  if (cell.rescaledValue > gridMax[trialCounter]) {
    gridMax[trialCounter] = cell.rescaledValue;
  }

  //keep track of tapped cell
  var d = new Date();
  tscollect[trialCounter][investigationIndex] = d.getTime();
  xcollect[trialCounter][investigationIndex] = cell.x;
  ycollect[trialCounter][investigationIndex] = cell.y;
  zcollect[trialCounter][investigationIndex] = cell.noiseyValue;
  zcollectScaled[trialCounter][investigationIndex] = cell.rescaledValue; 

  // update score
  scorecurrent = Math.round(cell.rescaledValue);
  scoretotal[trialCounter] = scoretotal[trialCounter] + scorecurrent;
  reward = rewardCum(scoretotal);
  roundScore = performanceScore(scoretotal[trialCounter], scale[trialCounter]);
  change('scoretotal', "Punktzahl: " + scoretotal[trialCounter]);
  

  
  // CASE: first (demo) trial
  if (trials == (totalTrialsNumber - 1) && clicks > 0) {
    return;
  }

  // CASE: first (demo) trial ended
  if (trials == (totalTrialsNumber - 1) && clicks == 0) {
    gridDeactivated = true;
    return;
  }



  // CASE: out of investigations
  if (clicks == 0) {
    starRating = starsEarned(roundScore);
    starArray[trialCounter] = parseFloat(starRating); //add to array
    //move to next page
    clickStart('page5', 'page5finished');
    //update trials remaining

    // last-1 trial (the one before bonus) just ended
    if (trials == 1) {
      document.getElementById("bonusIntroInstructions").style.display = "block";
      document.getElementById("bonusSidebarInstructions").style.display = "block";
      document.getElementById("sidebarContent").style.display = "none";
    }
    

    //calculate stars to award
    starDiv = "<div class=\"star-ratings-css\"><div class=\"star-ratings-css-top\" style=\"width: " + roundScore + "%\"></div><div class=\"star-ratings-css-bottom\"></div><br><br>"
    addToDiv("stars", starDiv)
    //Compile completion text
    var remainingMsg = (trials - 1) == 1 ? "weiteres Feld." : "weitere Felder.";
    completionDiv = "<br><br><br><br><h1 class=\"text-xl\">Du hast dieses Feld erkundet und hast <b>" 
      + starRating + " Sterne gesammelt</b>. Du hast noch " + (trials) + " " + remainingMsg + "</h1><br><br><br>" 
      + starDiv + "<br><br><br>";
    change("trials", completionDiv);
  }


  // CASE: last trial AND 10 investigations remaining
  if (trials == 0 && clicks == 10) {
    
    // show Bonus level instructions
    document.getElementById("gridDiv").style.display = "none";
    document.getElementById("alertGridDiv").style.display = "block";
    document.getElementById("progress").style.display = "none";

  }

  // CASE: last trial AND out of investigations
  if (trials == 0 && clicks == 0) {
      //Change button to move to completion page
      document.getElementById("nextTrialButton").onclick = clickStart('page5finished', 'page6');
      // Game over, calculate final score
      GameOverScore = finalPerformance(scoretotal);
      finalStarCount = totalStarsEarned(starArray);
      //update page6 div
      finalStarDiv = document.getElementById('stars').innerHTML;
      completionDiv = "<h1 class=\"text-xl\">Wow! Das hast du super gemacht! Und du hast insgesamt ganze <b>" + finalStarCount + " Sterne gesammelt</b>! Vielen Dank fürs Spielen!</b> </h1>" + finalStarDiv + "";
      change('thanksforcompleting', completionDiv);
  }
}








////////////////////////////////////////
// BONUS LEVEL
// //TODO: Refactor to Class
////////////////////////////////////////


var bonusLevelStep = 0;
var bonusCells = [];
var currentBonusCell = null;
var playingBonusRound = false;
var sliderMoved = false;
var confidenceSliderMoved = false;
function nextBonusLevelStep() {

  // bonus level init
  if (bonusLevelStep == 0) {
    playingBonusRound = true;
    bonusCells = Cell.getRandomCells(5);
    document.getElementById("gridDiv").style.display = "block";
    document.getElementById("alertGridDiv").style.display = "none";
    document.getElementById("bonusProgress").style.display = "block";
  }

  sliderMoved = false;
  confidenceSliderMoved = false;

  // Highlight next bonus cell
  currentBonusCell = bonusCells[bonusLevelStep];
  currentBonusCell.getTd().classList.toggle("border-blink"); 

}


function saveBonusStep() {

  var confidenceSlider = document.getElementById("confidenceSlider");
  var sicherValue = confidenceSlider != null ? confidenceSlider.value : null;

  
  var range = document.getElementById("valueSlider");
  var rangeValue = range != null ? range.value : null;

  if (sicherValue == null || !sliderMoved || !confidenceSliderMoved) {
    alert("Bitte gib Werte für beide Regler an.");
    return;
  }  

  // clear previous bonus cell
  if (currentBonusCell != null) {
    currentBonusCell.clearTempValue();
    currentBonusCell.getTd().classList.toggle("border-blink");
    currentBonusCell.getTd().classList.toggle("border-dashed");
  }

  //Save data
  bonusCollect.bonusCells[bonusLevelStep] = {
    "x": currentBonusCell.x,
    "y": currentBonusCell.y,
    "givenValue": parseInt(rangeValue),
    "howSecure": parseInt(sicherValue)
  }

  document.getElementById("bonusRemainingCounter").innerHTML = 5 - (bonusLevelStep + 1);

  // reset inputs
  confidenceSlider.value = 5;
  range.value = 25;

  bonusLevelStep++;

  // If last bonus round, then go back to the game
  if (bonusLevelStep == 5) return saveBonusLevel();

  // Else: go to next bonus round step
  nextBonusLevelStep();

}

function saveBonusLevel() {
  currentBonusCell = null;
  document.getElementById("bonusInstructions2").style.display = "block";
  document.getElementById("bonusInstructions").style.display = "none";

}

function chooseBonusCellToPlay() {

  for (var i = 0; i < bonusCells.length; i++) {
    bonusCells[i].getTd().classList.toggle("border-dashed");
  }

}

function onBonusCellTapped(cell) {

  if (bonusCells.indexOf(cell) == -1 || bonusLevelStep <= 4) return;

  var r = confirm("Bist du dir sicher?");
  if (!r) return;

  document.getElementById("bonusSidebarInstructions").style.display = "none";
  document.getElementById("bonusSidebarInstructions2").style.display = "block";

  playingBonusRound = false;
  for (var i = 0; i < bonusCells.length; i++) {
    bonusCells[i].getTd().classList.toggle("border-dashed");
  }

  bonusCollect.finalChosenCell = {
    "x": cell.x,
    "y": cell.y,
    "scaledValue": cell.scaledValue
  }

  onCellTapped(cell);

  document.getElementById("gridDiv").style.display = "block";
  document.getElementById("bonusProgress").style.display = "none";
  document.getElementById("progress").style.display = "block";
}

function onValueSliderChange(value) {
  if (currentBonusCell != null)
    currentBonusCell.setTempValue(value);

  sliderMoved = true;
}

function onConfidenceSliderChange(value) {
  confidenceSliderMoved = true;
}




////////////////////////////////////////
// Cell class
////////////////////////////////////////


/**
 * Cell Class
 * 
 * @param {number} x 
 * @param {number} y 
 * @param {number} aValue 
 * @param {number} nValue 
 * @param {number} rValue 
 */
function Cell(x, y, aValue, nValue, rValue) {

  /**
   * Cell properties
   */

  this.x = x;
  this.y = y;
  this.absoluteValue = aValue;
  this.noiseyValue = nValue;
  this.rescaledValue = rValue;
  this.history = [];
  

  /**
   * Cell methods
   */

  this.getTd = function () {
    var cells = document.querySelectorAll('td[data-x="' + this.x + '"][data-y="' + this.y + '"]');
    if (cells.length > 0) return cells[0];
    else return null;
  }

  /*this.getId = function () {
    var td = this.getTd();
    if (td == null) return null;
    return td.getAttribute("id");
  }*/

  this.updateValue = function () {
    this.noiseyValue = Math.round(this.absoluteValue + myNorm());
    newRescaledValue = Math.max(Math.round(this.noiseyValue / 50 * scale[trialCounter] + 5), 0);
    this.rescaledValue = newRescaledValue;
    var bgcolor = heatmapColor(Math.round(average(this.history)));
    this.setBgColor(bgcolor);
  }

  this.addToHistory = function (value) {
    this.history.push(value);
    if (this.getTd()) this.getTd().setAttribute("title", this.history.toString());
  }

  this.hasHistory = function () {
    return this.history.length > 0;
  }

  this.setBgColor = function (color) {
    if (this.getTd()) this.getTd().style.backgroundColor = color;
  }

  this.clicked = function () {
    var td = this.getTd();
    if (this.hasHistory()) {
      this.updateValue();
    } else {
      if (td) td.setAttribute("title", this.rescaledValue.toString());
      var bgcolor = heatmapColor(this.rescaledValue);
      this.setBgColor(bgcolor);
    }
    this.addToHistory(this.rescaledValue);
    if (td) {
      td.innerHTML = this.rescaledValue;
      td.classList.toggle("highlight");
      setTimeout(function () {
        td.classList.toggle("highlight");
      }, 300);
    } 
  }

  this.setTempValue = function (value) {
    if (this.getTd()) this.getTd().innerHTML = value;
    var bgcolor = heatmapColor(value);
    this.setBgColor(bgcolor);
  }

  this.clearTempValue = function (value) {
    if (this.getTd()) this.getTd().innerHTML = "";
    this.setBgColor("transparent");
  }



  /**
   * Static properties and methods
   */

  Cell.updateEnvironment = function() {
    Cell.cells = []; 
    for (i = 0; i < 8; i++) {
      Cell.cells[i] = [];
    }
    console.log("trialCounter in updateEnvironment() is: " + trialCounter);
    var env = environmentList[envOrder[trialCounter]];
    for (var k = 0; k <= 63; k++) {
      var x = env[k].x1;
      var y = env[k].x2;
      var absoluteValue = env[k].y * 50;
      var noiseyValue = Math.round(absoluteValue + myNorm());
      var rescaledValue = Math.max(Math.round(noiseyValue / 50 * scale[trialCounter] + 5), 0);
      var cell = new Cell(x, y, absoluteValue, noiseyValue, rescaledValue);
      Cell.cells[x][y] = cell;
    }
  }

  Cell.getCell = function(x, y) {
    return Cell.cells[x][y];
  }

  Cell.getRandomCell = function() {
    var x = Math.floor(Math.random() * Cell.cells.length);
    var y = Math.floor(Math.random() * Cell.cells[x].length);
    return Cell.getCell(x, y);
  }

  Cell.getRandomCells = function(n) {
    randomCells = [];
    for (var i = 0; i < n; i++) {
      var found = false;
      while (!found) {
        var cell = this.getRandomCell();
        if (!cell.hasHistory() && randomCells.indexOf(cell) == -1) {
          randomCells.push(cell);
          found = true;
        }
      }
    }
    return randomCells;
  }


}
new Cell();







function nexttrial() {

  console.log("nexttrial called");

  //proceed only if there are more trials available

  trials = trials - 1; // decrease remaining trials
  console.log("[Debug] You have now " +trials+ " reamaining trials");

  if (trials >= 0) {
    initcollect[trialCounter] = init; //retrieve initially revealed tile from previous trial before updating trial counter
    
    //update trialCounter
    trialCounter = trialCounter + 1;

    createGrid();
    Cell.updateEnvironment();
    var firstCell = Cell.getRandomCell();
    firstCell.clicked();

    //store initial values
    var d = new Date();
    tscollect[trialCounter][0] = d.getTime();
    xcollect[trialCounter][0] = firstCell.x;
    ycollect[trialCounter][0] = firstCell.y;
    zcollect[trialCounter][0] = firstCell.noiseyValue;
    zcollectScaled[trialCounter][0] = firstCell.rescaledValue; //store noisey value

    //update gridMax with initial tile
    gridMax[trialCounter] = firstCell.rescaledValue;
    scoretotal[trialCounter] = firstCell.rescaledValue;
    //Update text 
    change('scoretotal', "Punktzahl: " + scoretotal[trialCounter]);
    //go back to task
    clickStart('page5finished', 'page5');
    //renew investigations
    clicks = horizon;
    //renew investigationIndex
    investigationIndex = 0;
    //update current reward, number of trials and clicks
    change("remaining1", "Verbleibende Spielfelder: <b>" + (trials + 1) + "</b>");
    change("remaining2", "Verbleibende Klicks: <b>" + clicks + "</b>");
    //if out of trials go to next page
  }

  // If remaining trials < 0 --> game ended
  if (trials < 0) {
    //move to final page
    clickStart('page5', 'page6');
  }
}

function debugData() {
  console.log(tscollect);
  console.log(xcollect);
  console.log(ycollect);
  console.log(zcollect);
}

function senddata() { //TODO: set up for tablet
  //search history
  searchHistory = {
    'tscollect': tscollect,
    'xcollect': xcollect,
    'ycollect': ycollect,
    'zcollect': zcollect,
    'zcollectScaled': zcollectScaled
  };
  //All data to save
  saveDataArray = {
    'condition': condition,
    'scale': scale,
    'envOrder': envOrder,
    'searchHistory': searchHistory,
    'bonusLevel': bonusCollect,
    'starArray': starArray,
    'age': age,
    'gender': gender,
    'grade': grade,
    'testerNotes': testerNotes
  };

  incrementCounter();

  console.log("[DEBUG] SavaData Array:");
  console.log(saveDataArray);

  isrcUtils.SaveAndEnd(saveDataArray);
  clickStart('page7', 'rt-end');

}

//*************UTILITIES***************************************


//changes from one page to another
function clickStart(hide, show) {
  document.getElementById(hide).style.display = "none";
  document.getElementById(show).style.display = "block";
  window.scrollTo(0, 0);
}

//changes inner HTML of div with ID=x to y
function change(x, y) {
  document.getElementById(x).innerHTML = y;
}

//adds y to inner HTML of div with ID=x
function addToDiv(x, y) {
  document.getElementById(x).innerHTML += y;
}

//Function to randomly shuffle an array:
function shuffle(o) { //v1.0
  for (var j, x, i = o.length; i; j = Math.floor(Math.random() * i), x = o[--i], o[i] = o[j], o[j] = x);
  return o;
};

//Randomly sample n values from an array
function getRandomSubarray(arr, size) {
  var shuffled = arr.slice(0),
    i = arr.length,
    temp, index;
  while (i--) {
    index = Math.floor((i + 1) * Math.random());
    temp = shuffled[index];
    shuffled[index] = shuffled[i];
    shuffled[i] = temp;
  }
  return shuffled.slice(0, size);
}

//load JSON file
function loadJSON(file, callback) {
  var rawFile = new XMLHttpRequest();
  rawFile.overrideMimeType("application/json");
  rawFile.open("GET", file, true);
  rawFile.onreadystatechange = function () {
    if (rawFile.readyState === 4 && rawFile.status == "200") {
      callback(rawFile.responseText);
    }
  }
  rawFile.send(null);
}

//Create normal noise distribution
function myNorm() {
  var x1, x2, rad, c;
  do {
    x1 = 2 * Math.random() - 1;
    x2 = 2 * Math.random() - 1;
    rad = x1 * x1 + x2 * x2;
  } while (rad >= 1 || rad == 0);
  c = Math.sqrt(-2 * Math.log(rad) / rad);
  return (x1 * c);
};

//average the values in an array
function average(inputArray) {
  var total = 0
  for (var i = 0; i < inputArray.length; i++) {
    total += inputArray[i];
  }
  var avg = total / inputArray.length;
  return avg;
};

//Convert cumulative score to reward value
function rewardCum(scoreTotal) {
  var r = 0,
    r_i;
  for (var i = 0; i < scoreTotal.length; i++) {
    r_i = scoreTotal[i] / (scale[i] + 5) / 300 * 1.5;
    r = r + r_i
  }
  if (r > 1.5) {
    r = 1.5; //limit to max reward, in case of any funny business
  }
  return toFixed(r, 2);
}


//single trial reward
function performanceScore(points, scale) {
  var r = 0;
  //cumulative regret (as a percentage)
  r = points / ((scale + 5) * horizon);
  return toFixed(r * 100);
}

function finalPerformance(scoreArray) {
  var finalScore = 0;
  for (i = 0; i < scoreArray.length; i++) { //loop through score array
    finalScore += parseInt(performanceScore(parseInt(scoreArray[i]), parseInt(scale[i])));
  }
  return toFixed(finalScore / scoreArray.length)
}
//calculate number of stars
function starsEarned(score) {
  //console.log("score: " + score);

  percentageScore = score / 100;
  //console.log("percentageScore: " + percentageScore);

  scoreOutOfFive = percentageScore * 5;
  //console.log("scoreOutOfFive: " + scoreOutOfFive);

  fixedScoreOutOfFive =  toFixed(scoreOutOfFive, 1);
  //console.log("fixedScoreOutOfFive", fixedScoreOutOfFive)

  return parseInt(fixedScoreOutOfFive) >= 5 ? 5 : fixedScoreOutOfFive;
}

function totalStarsEarned(starArray) {
  var totalStars = 0;
  for (i = 0; i < starArray.length; i++) { //loop through score array
    totalStars += parseFloat(starArray[i]);
  }
  return toFixed(totalStars)
}


//random number generator
function randomNum(min, max) {
  return Math.floor(Math.random() * (max - min + 1) + min)
}

//Display a float to a fixed percision
function toFixed(value, precision) {
  var precision = precision || 0,
    power = Math.pow(10, precision),
    absValue = Math.abs(Math.round(value * power)),
    result = (value < 0 ? '-' : '') + String(Math.floor(absValue / power));

  if (precision > 0) {
    var fraction = String(absValue % power),
      padding = new Array(Math.max(precision - fraction.length, 0) + 1).join('0');
    result += '.' + padding + fraction;
  }
  //console.log(result);
  return result;
}

// extract URL parameters (FROM: https://s3.amazonaws.com/mturk-public/externalHIT_v1.js)
function turkGetParam(name) {
  var regexS = "[\?&]" + name + "=([^&#]*)";
  var regex = new RegExp(regexS);
  var tmpURL = fullurl;
  var results = regex.exec(tmpURL);
  if (results == null) {
    return "";
  } else {
    return results[1];
  }
}

function getAge(birthDate) {

  var dob = new Date(birthDate);
  var today = new Date();
  var age2 = today.getFullYear() - dob.getFullYear();
  var m = today.getMonth() - dob.getMonth();
  if (m < 0 || (m === 0 && today.getDate() < dob.getDate())) age2--;
  return age2;
}

function getCounter(counterName) {
  if (localStorage.getItem(counterName) === null)
    return 0;
  else
    return parseInt(localStorage.getItem(counterName));
}

function incrementCounter() {
  var counter = 0;
  if (age <= 9) {
    counter = getCounter("gridsearch-counter-1");
    localStorage.setItem("gridsearch-counter-1", counter + 1);
  } else if (age > 9 && age < 18) {
    counter = getCounter("gridsearch-counter-2");
    localStorage.setItem("gridsearch-counter-2", counter + 1);
  } else if (age > 18) {
    counter = getCounter("gridsearch-counter-3");
    localStorage.setItem("gridsearch-counter-3", counter + 1);
  }
}

//END