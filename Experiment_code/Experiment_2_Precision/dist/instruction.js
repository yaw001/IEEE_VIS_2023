function pageLoad() {
    document.getElementById('practice').style.display = 'none';
    document.getElementById('trials').style.display = 'none';
    document.getElementById('position').style.display = 'none';
    document.getElementById('clockdiv').style.display = "none";
    document.getElementById('consent').style.display = 'block';
}

function clickConsent() {
    document.getElementById('instruction').style.display = 'block';
    document.getElementById('consent').style.display = 'none';
}

function clickPractice() {
    document.getElementById('instruction').style.display = 'none';
    document.getElementById('practice').style.display = 'block';
}

function clickPractice_num() {
    document.getElementById('instruction_num').style.display = 'none';
    document.getElementById('practice').style.display = 'block';
}

function clickTrials() {
    document.getElementById('practice').style.display = 'none';
    document.getElementById('position').style.display = 'block';
}



// function pageLoad() {
//     document.getElementById('practice').style.display = 'none';
//     document.getElementById('trials').style.display = 'none';
//     document.getElementById('position').style.display = 'position';
//     document.getElementById('clockdiv').style.display = "none";
//     document.getElementById('consent').style.display = 'none';
// }