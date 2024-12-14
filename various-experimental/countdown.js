Qualtrics.SurveyEngine.addOnload(function() {
    var totalTime = 300; 
    var timeLeft = totalTime; 
    var timerDiv = document.createElement("div");
    timerDiv.id = "timer";
    timerDiv.style.fontSize = "20px";
    timerDiv.style.textAlign = "center";
    timerDiv.innerHTML = "Time remaining: " + timeLeft + " seconds";
    document.getElementById("Questions").appendChild(timerDiv);

    // finding the button and the arrow
    var nextButton = jQuery(".NextButton, #NextButton");
    var nextButtonArrow = jQuery(".Skin .NextButton"); 

    // disabling both the button and the arrow initially
    nextButton.prop("disabled", true);
    nextButtonArrow.css("pointer-events", "none"); // prevent clicking on the arrow
    nextButtonArrow.css("opacity", "0.5"); 

    //countdown
    var interval = setInterval(function() {
        timeLeft--;
        timerDiv.innerHTML = "Time remaining: " + timeLeft + " seconds";

        // enabling the button after 60s
        if (timeLeft === totalTime - 60) {
            nextButton.prop("disabled", false);
            nextButtonArrow.css("pointer-events", "auto"); 
            nextButtonArrow.css("opacity", "1"); 
        }


        if (timeLeft <= 0) {
            clearInterval(interval);
            timerDiv.innerHTML = "You may now proceed.";
        }
    }, 1000); 
});
