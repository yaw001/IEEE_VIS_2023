
const split = 206;
var startTime;
var endTime;
var response_time;
// var response_time_list = [];
function stimuli(trial_number) {
    coord = get_positions(trial_index[trial_number], trial_number);
    size_1 = trial_index[trial_number][0];
    size_2 = trial_index[trial_number][1];
    console.log(coord);
    group_1_coord = coord.group_1_coord;
    group_2_coord = coord.group_2_coord;
    all_coord = coord.all_coord;
    if (size_1 == 1) {
        mean_group_1 = group_1_coord;
    } else {
        mean_group_1 = compute_2d_mean(group_1_coord);
    }
    if (size_2 == 1) {
        mean_group_2 = group_2_coord;
    } else {
        mean_group_2 = compute_2d_mean(group_2_coord);
    }
    mean_all_coord = compute_2d_mean(all_coord);
}

function draw(group_1_coord, group_2_coord, radius, color, size_1, size_2) {
    draw_group_1(group_1_coord, radius, color, size_1);
    draw_group_2(group_2_coord, radius, color, size_2);
}

function draw_group_1(group_1_coord, radius, color, size_1) {
    if (size_1 == 1) {
        circle(group_1_coord.x, group_1_coord.y, radius, color);
    } else {
        group_1_coord.map(x => circle(x.x, x.y, radius, color));
    }
}

function draw_group_2(group_2_coord, radius, color, size_2) {
    if (size_2 == 1) {
        circle(group_2_coord.x, group_2_coord.y, radius, color);
    } else {
        group_2_coord.map(x => circle(x.x, x.y, radius, color));
    }
}

var response = {};
getClickPosition = function (e) {
    var offset_x;
    var offset_y;
    var response_x;
    var response_y;
    if (on) {
        offset_x = canvas.offsetLeft;
        offset_y = canvas.offsetTop;
        response_x = e.clientX - offset_x;
        response_y = e.clientY - offset_y;
        setInterval(response_cross(response_x, response_y, radius, "red"), 5);
        ctx.globalCompositeOperation = "destination-over";
        draw(group_1_coord, group_2_coord, radius, color, size_1, size_2);
        btn.innerHTML = "SUBMIT";
        btn.disabled = false;
    }
    return response = {
        x: response_x,
        y: response_y
    };
};

var num_response = {};
getNumberInput = function () {
    var num_response_1;
    var num_response_2;
    var num_input_1 = document.getElementById("input_1");
    var num_input_2 = document.getElementById("input_2");
    num_response_1 = num_input_1.value;
    num_response_2 = num_input_2.value;
    return num_response = {
        num_response_1: parseInt(num_response_1),
        num_response_2: parseInt(num_response_2)
    };
};

function getTimeRemaining(endtime) {
    const total = Date.parse(endtime) - Date.parse(new Date());
    const seconds = Math.floor((total / 1000) % 60);
    const minutes = Math.floor((total / 1000 / 60) % 60);
    // const hours = Math.floor((total / (1000 * 60 * 60)) % 24);
    // const days = Math.floor(total / (1000 * 60 * 60 * 24));

    return {
        total,
        // days,
        // hours,
        minutes,
        seconds
    };
}

function initializeClock(id, endtime) {
    const clock = document.getElementById(id);
    // const daysSpan = clock.querySelector('.days');
    // const hoursSpan = clock.querySelector('.hours');
    const minutesSpan = clock.querySelector('.minutes');
    const secondsSpan = clock.querySelector('.seconds');

    function updateClock() {
        const t = getTimeRemaining(endtime);

        // daysSpan.innerHTML = t.days;
        // hoursSpan.innerHTML = ('0' + t.hours).slice(-2);
        minutesSpan.innerHTML = ('0' + t.minutes).slice(-2);
        secondsSpan.innerHTML = ('0' + t.seconds).slice(-2);

        if (t.total <= 0) {
            clearInterval(timeinterval);
            document.getElementById('clockdiv').style.display = 'none';
            document.getElementById('position').style.display = 'block';
            clear();
            trial();
        }
    }
    updateClock();
    const timeinterval = setInterval(updateClock, 1000);
}

// button behavior
document.querySelector("#btn_p").addEventListener('click', function () {
    document.getElementById('practice').style.display = 'none';
    document.getElementById('position').style.display = 'block';
    clear();
    trial()
});

document.querySelector('#btn_t').addEventListener('click', function () {
    document.getElementById('trials').style.display = 'none';
    document.getElementById('position').style.display = 'block';
    clear();
    trial();
});

document.querySelector("#btn").addEventListener('click', function () {
    if (btn.innerHTML == "START") {
        trial();
        btn.disabled = true;
    } else if (btn.innerHTML == "SUBMIT") {
        response_coord = response;
        ctx.globalCompositeOperation = "source-over";
        on = false;
        btn.innerHTML = "NEXT";
        endTime = Date.now();
        response_time = endTime - startTime;
        // response_time_list.push(response_time);
        console.log(response_time)
        // clear();
    } else if (btn.innerHTML == "NEXT") {
        document.getElementById('canvas').style.pointerEvents = 'none';
        on = true;
        btn.disabled = true;
        trialDone();
    }
});


function trialDone() {
    trialData.push({
        trialNumber: trialNumber,
        // rand_index_1: rand_index_1,
        // rand_index_2: rand_index_2,
        group_1_size: trial_index[trialNumber][0],
        group_2_size: trial_index[trialNumber][1],
        group_sd: trial_index[trialNumber][2],
        mean_index: trial_index[trialNumber][3],
        group_1_coord: group_1_coord,
        group_2_coord: group_2_coord,
        all_coordinates: all_coord,
        response_coord: response_coord,
        mean_group_1: mean_group_1,
        mean_group_2: mean_group_2,
        mean_all: mean_all_coord,
        inner_width: innerWidth,
        inner_height: innerHeight,
        response_time:response_time
    });
    trialNumber++;

    console.log(trialData);

    if (trialNumber < expt.maxTrials) {
        if (trialNumber == 6) {
            document.getElementById('position').style.display = 'none';
            document.getElementById('trials').style.display = 'block';
        } else if (trialNumber == split) {
            document.getElementById('position').style.display = 'none';
            document.getElementById('clockdiv').style.display = "block";
            const deadline = new Date(Date.parse(new Date()) + 60 * 1000);
            initializeClock('clockdiv', deadline);
        } else {
            clear();
            trial();
        }  
    } else {
        data = { client: client, trials: trialData };
        writeServer(data);
        // document.getElementById('survey').style.display = "none";
        document.getElementById('position').style.display = 'none';
        document.getElementById('completed').style.display = "block";
    }
    return trialNumber;
}

function experimentDone() {
    submitExternal(client);
}

function trial() {
    stimuli(trialNumber);
    startTime = Date.now();
    setTimeout(() => { draw(group_1_coord, group_2_coord, radius, color, size_1, size_2); }, 600);
    // setTimeout(() => { clear(); }, 1000);
    setTimeout(() => { document.getElementById('canvas').style.pointerEvents = 'auto'; }, 605);
    setTimeout(() => { document.querySelector("canvas").addEventListener("click", getClickPosition); }, 610);
}