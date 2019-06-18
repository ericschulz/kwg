/**
 * Stimuli
 */
var Stimuli = {


    /**
     * Properties
     */
    buttons: [],
    preloadedImages: [],
    player: null,
    currentToy: -1,
    currentQuiz: -1,

    instructorsConditions: [
        ["active", "instructive", "passive"],
        ["active", "passive", "instructive"],
        ["instructive", "active", "passive"],
        ["instructive", "passive", "active"],
        ["passive", "instructive", "active"],
        ["passive", "active", "instructive"]
    ],

    toysConditions: [
        ["same", "related", "unrelated"],
        ["same", "unrelated", "related"],
        ["related", "same", "unrelated"],
        ["related", "unrelated", "same"],
        ["unrelated", "same", "related"],
        ["unrelated", "related", "same"],
    ],

    instructors: [
        {
            "color": "yellow",
            "condition": null,
            "howLearnedMultiple": null,
            "howLearnedText": null
        },
        {
            "color": "red",
            "condition": null,
            "howLearnedMultiple": null,
            "howLearnedText": null
        },
        {
            "color": "blue",
            "condition": null,
            "howLearnedMultiple": null,
            "howLearnedText": null
        }
    ],

    toys: [
        {
            "condition": null,
            "chosenInstructor": null
        },
        {
            "condition": null,
            "chosenInstructor": null
        },
        {
            "condition": null,
            "chosenInstructor": null
        }
    ],



    /**
     * Cordova: initialize()
     */
    initialize: function () {
        document.addEventListener('deviceready', this.onDeviceReady.bind(this), false);
    },


    /**
     * Cordova: onDeviceReady()
     */
    onDeviceReady: function () {
        this.receivedEvent('deviceready');
    },


    /**
     * Cordova: receivedEvent()
     */
    receivedEvent: function (id) {

        switch (id) {
            case 'deviceready':
                this.Setup();
                Stimuli.SetHandlers();
                break;
        }


    },


    /**
     * Setup()
     */
    Setup: function () {

        Stimuli.player =  document.getElementById("video-player");

        // Set instructors conditions (counterbalanced)
        Stimuli.instructors[0].condition = Stimuli.instructorsConditions[isrcUtils.GetCounter() % 6][0];
        Stimuli.instructors[1].condition = Stimuli.instructorsConditions[isrcUtils.GetCounter() % 6][1];
        Stimuli.instructors[2].condition = Stimuli.instructorsConditions[isrcUtils.GetCounter() % 6][2];

        // Set toys conditions (counterbalanced, unrelated to instructors)
        Stimuli.toys[0].condition = Stimuli.toysConditions[isrcUtils.GetCounter() % 6][0];
        Stimuli.toys[1].condition = Stimuli.toysConditions[isrcUtils.GetCounter() % 6][1];
        Stimuli.toys[2].condition = Stimuli.toysConditions[isrcUtils.GetCounter() % 6][2];

        // Preload images
        var imgs = [
            "img/instructors/yellow.png", "img/instructors/red.png", "img/instructors/blue.png",
            "img/toys/same.png", "img/toys/related.png", "img/toys/unrelated.png"
        ]
        isrcUtils.PreloadImages(imgs);

    },


    /**
     * SetHandlers()
     */
    SetHandlers: function () {

        Stimuli.buttons[0] = document.getElementById('instructor-yellow');
        Stimuli.buttons[0].addEventListener('click', Stimuli.InstructorClicked);
        Stimuli.buttons[1] = document.getElementById('instructor-red');
        Stimuli.buttons[1].addEventListener('click', Stimuli.InstructorClicked);
        Stimuli.buttons[2] = document.getElementById('instructor-blue');
        Stimuli.buttons[2].addEventListener('click', Stimuli.InstructorClicked);

        Stimuli.player.addEventListener("ended", Stimuli.VideoEndedHandler);

        Stimuli.buttons[3] = document.getElementById('button-toy-begin');
        Stimuli.buttons[3].addEventListener('click', Stimuli.ShowNextToy);

        Stimuli.buttons[5] = document.getElementById('ask-help-yellow');
        Stimuli.buttons[5].addEventListener('click', function (evt) {
            isrcUtils.RequestConfirmation(Stimuli.AskForHelp, evt);
        });
        Stimuli.buttons[6] = document.getElementById('ask-help-red');
        Stimuli.buttons[6].addEventListener('click', function (evt) {
            isrcUtils.RequestConfirmation(Stimuli.AskForHelp, evt);
        });
        Stimuli.buttons[7] = document.getElementById('ask-help-blue');
        Stimuli.buttons[7].addEventListener('click', function (evt) {
            isrcUtils.RequestConfirmation(Stimuli.AskForHelp, evt);
        });
        
        Stimuli.buttons[8] = document.getElementById('button-quiz-begin');
        Stimuli.buttons[8].addEventListener('click', Stimuli.ShowNextQuiz);

        Stimuli.buttons[9] = document.getElementById('button-answer-quiz');
        Stimuli.buttons[9].addEventListener('click', function (evt) {
            isrcUtils.RequestConfirmation(Stimuli.AnswerQuiz, evt);
        });

    },


    /**
     * InstructorClicked()
     */
    InstructorClicked: function (evt) {
        var instructorIndex = parseInt(evt.target.getAttribute("data-instructor-index"));
        var instructorColor = Stimuli.instructors[instructorIndex].color;
        var instructorCondition = Stimuli.instructors[instructorIndex].condition;
        Stimuli.player.src = "videos/" + instructorColor + "-" + instructorCondition + ".mp4";
        Stimuli.player.play();
        isrcUtils.Goto("rt-play-video");
    },


    /**
     * VideoEndedHandler()
     */
    VideoEndedHandler: function () {
        // if last video (blue instructor)...
        if (Stimuli.player.getAttribute("src").indexOf("blue") !== -1) 
            return isrcUtils.Goto("rt-toy-begin");
        else
            return isrcUtils.Goto("rt-choose-instructor");
    },


    /**
     * ShowNextToy()
     */
    ShowNextToy: function () {
        Stimuli.currentToy++;

        if (Stimuli.currentToy >= Stimuli.toys.length)
            return isrcUtils.Goto("rt-quiz-begin");

        var src = "img/toys/" + Stimuli.toys[Stimuli.currentToy].condition + ".png";
        document.getElementById("toy-img").setAttribute("src", src);
        isrcUtils.Goto("rt-show-toy");
    },


    /**
     * AskForHelp()
     */
    AskForHelp: function (evt) {
        var instructorIndex = evt.target.getAttribute("data-instructor-index");
        Stimuli.toys[Stimuli.currentToy].chosenInstructor = Stimuli.instructors[instructorIndex].color;
        Stimuli.ShowNextToy();
    },


    /**
     * ShowNextQuiz()
     */
    ShowNextQuiz: function () {
        Stimuli.currentQuiz++;
        Stimuli.ResetQuiz();

        if (Stimuli.currentQuiz >= Stimuli.instructors.length)
            return Stimuli.End();

        var src = "img/instructors/" + Stimuli.instructors[Stimuli.currentQuiz].color + ".png";
        document.getElementById("quiz-img").setAttribute("src", src);
        isrcUtils.Goto("rt-quiz");
    },


    /**
     * AnswerQuiz
     */
    AnswerQuiz: function () {
        var text = document.getElementById("howlearned-text").value;
        var choice = document.getElementById("howlearned-multiple").value;
        Stimuli.instructors[Stimuli.currentQuiz].howLearnedText = text;
        Stimuli.instructors[Stimuli.currentQuiz].howLearnedMultiple = choice;
        Stimuli.ShowNextQuiz();
    },


    /**
     * ResetQuiz
     */
    ResetQuiz: function () {
        document.getElementById("howlearned-text").value = "";
        document.getElementById("howlearned-multiple").value = "";
    },


    /**
     * End()
     */
    End: function () {

        // Add data to save in the DB
        var data ={
            "instructorsCondition": Stimuli.instructors[0].condition.charAt(0) +
                Stimuli.instructors[1].condition.charAt(0) + Stimuli.instructors[2].condition.charAt(0),
            "toysCondition": Stimuli.toys[0].condition.charAt(0) +
                Stimuli.toys[1].condition.charAt(0) + Stimuli.toys[2].condition.charAt(0),
            "instructors": Stimuli.instructors,
            "toys": Stimuli.toys
        };

        // End the experiment
        isrcUtils.SaveAndEnd(data);

    }


}
Stimuli.initialize();