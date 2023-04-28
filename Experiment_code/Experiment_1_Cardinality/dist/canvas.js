var client = parseClient();
//trial info
var trialNumber = 0;
var trialData = [];

//canvas parameters
var canvas = document.getElementById("canvas");
var ctx = canvas.getContext("2d");

const width_height = Math.min(innerWidth, innerHeight);
console.log(innerHeight)
canvas.width = innerHeight * 0.99;
// canvas.width = innerWidth * 0.6;
canvas.height = innerHeight * 0.99;
const width_center = canvas.width / 2;
const height_center = canvas.height / 2;
console.log(canvas.width);


// circle stimuli size
var radius = canvas.height / 200;

var group_range_unit = canvas.height / 8;
var group_mean_to_center = canvas.height / 8 * 2.5;

//object features
// var color_set = ["#00ced1", "#ffa500", "#00ff00", "#0000ff", "#ff1493"];
// var color_index = jStat.seq(0, color_set.length - 1, color_set.length);
// var color = "#0000ff";
var color = "black";
// var shape_set = ["circle", "rectangle", "triangle"];
var shape = "circle";

//response circle
var on = true;


//coordinate initialization
var coord;
var rotated_coord;
var group_1_coord;
var group_2_coord;
var all_coord;

// var mean_group_1_resize;
// var mean_group_2_resize;
// var all_coord_resize;
var response_ratio = {};
var is_resize = 0;

// cardinality given approximately equal axial precision
// 18 conditions 
size_pairs = [
    [1, 1],
    [1, 2],
    [1, 4],
    [1, 8],
    [1, 16],
    [2, 2],
    [2, 4],
    [2, 8],
    [2, 16],
    [2, 32],
    [4, 4],
    [4, 8],
    [4, 16],
    [4, 32],
    [4, 64],
    [8, 8],
    [8, 16],
    [8, 32],
    [8, 64],
    [8, 128]
];

// Varying overall precision
range_multiplier = [1];

// Indexing
num_trials = 20
var means_index = jStat.seq(0, num_trials - 1, num_trials);
var cuts = 360 / num_trials

// Indexing rule: [group_1_size, group_2_size, range_multiplier, mean_index]

// Practice trial
// var practice_1 = [4, 4, 1, -2];
// var practice_2 = [1, 8, 1, -6];
// var practice_3 = [2, 16, 1, -10];
// var practice_4 = [8, 128, 1, -14];
// var practice_5 = [4, 8, 1, -18];
// var practice_6 = [8, 16, 1, -22];

var practice_1 = [1, 2, 1, 0];
var practice_2 = [4, 8, 1, 0];
var practice_3 = [16, 32, 1, 0];
var practice_4 = [64, 128, 1, 0];
var practice_5 = [8, 128, 1, 0];
var practice_6 = [1, 2, 1, 0];

// Trial structure: 6 practice trials for each part + 20*20 = 400 (experimental trials)
var trial_size_pairs = repeat_each_array(size_pairs, num_trials);
var trial_size_sd = repeat_each_single(range_multiplier, 20*num_trials);
var trial_means_index = repeat_whole_single(means_index,20);
var trial_index = combineArray(trial_size_pairs, trial_size_sd);
trial_index = addArray(trial_index, trial_means_index);


trial_index = shuffle(trial_index);

trial_index.splice(0, 0, practice_6);
trial_index.splice(0, 0, practice_5);
trial_index.splice(0, 0, practice_4);
trial_index.splice(0, 0, practice_3);
trial_index.splice(0, 0, practice_2);
trial_index.splice(0, 0, practice_1);


console.log(trial_index)

// trial_index = combineArray(trial_index, sub_canvas_center_list);


// samples
const samples = JSON.parse(all_samples);

//random index
const size_index_1 = jStat.seq(0, 99, 100);
const size_index_2 = jStat.seq(0, 99, 100);

var rand_index_1;
var rand_index_2;

var rand_index_1_list = [];
var rand_index_2_list = [];

// group position generator
function get_positions(trial_index) {
    rand_index_1 = randSample(clone(size_index_1), 1);
    rand_index_2 = randSample(clone(size_index_2), 1);
    // rand_index_1_list.push(rand_index_1);
    // rand_index_2_list.push(rand_index_2);
    switch (trial_index[0]) {
        case 1:
            group_1_coord = {
                x: 0,
                y: 0
            };
            break;
        case 2:
            group_1_coord = clone(samples[0].group_sample_1[rand_index_1]);
            break;
        case 4:
            group_1_coord = clone(samples[1].group_sample_1[rand_index_1]);
            break;
        case 8:
            group_1_coord = clone(samples[2].group_sample_1[rand_index_1]);
            break;
        case 16:
            group_1_coord = clone(samples[3].group_sample_1[rand_index_1]);
            break;
        case 32:
            group_1_coord = clone(samples[4].group_sample_1[rand_index_1]);
            break;
        case 64:
            group_1_coord = clone(samples[5].group_sample_1[rand_index_1]);
            break;
        case 128:
            group_1_coord = clone(samples[6].group_sample_1[rand_index_1]);
            break;
    }
    switch (trial_index[1]) {
        case 1:
            group_2_coord = {
                x: 0,
                y: 0
            };
            break;
        case 2:
            group_2_coord = clone(samples[0].group_sample_2[rand_index_2]);
            break;
        case 4:
            group_2_coord = clone(samples[1].group_sample_2[rand_index_2]);
            break;
        case 8:
            group_2_coord = clone(samples[2].group_sample_2[rand_index_2]);
            break;
        case 16:
            group_2_coord = clone(samples[3].group_sample_2[rand_index_2]);
            break;
        case 32:
            group_2_coord = clone(samples[4].group_sample_2[rand_index_2]);
            break;
        case 64:
            group_2_coord = clone(samples[5].group_sample_2[rand_index_2]);
            break;
        case 128:
            group_2_coord = clone(samples[6].group_sample_2[rand_index_2]);
            break;
    }
    //recenter
    if (trial_index[0] == 1 && trial_index[1] == 1) {
        group_1_coord.x = width_center + group_mean_to_center * Math.cos(degToRad(180 + trial_index[3] * cuts));
        group_1_coord.y = height_center + group_mean_to_center * Math.sin(degToRad(180 + trial_index[3] * cuts));

        group_2_coord.x = width_center + group_mean_to_center * Math.cos(degToRad(trial_index[3] * cuts));
        group_2_coord.y = height_center + group_mean_to_center * Math.sin(degToRad(trial_index[3] * cuts));
        all_coord = [group_1_coord].concat([group_2_coord]);
    } else if (trial_index[0] != 1 && trial_index[1] == 1) {
        group_1_coord.map(x => x.x = (width_center + group_mean_to_center * Math.cos(degToRad(180 + trial_index[3] * cuts))) + x.x * trial_index[2] * (group_range_unit));
        group_1_coord.map(x => x.y = height_center + group_mean_to_center * Math.sin(degToRad(180 + trial_index[3] * cuts)) + x.y * trial_index[2] * (group_range_unit));

        group_2_coord.x = width_center + group_mean_to_center * Math.cos(degToRad(trial_index[3] * cuts));
        group_2_coord.y = height_center + group_mean_to_center * Math.sin(degToRad(trial_index[3] * cuts));

        all_coord = group_1_coord.concat([group_2_coord]);
    } else if (trial_index[0] == 1 && trial_index[1] != 1) {
        group_1_coord.x = width_center + group_mean_to_center * Math.cos(degToRad(180 + trial_index[3] * cuts));
        group_1_coord.y = height_center + group_mean_to_center * Math.sin(degToRad(180 + trial_index[3] * cuts));

        group_2_coord.map(x => x.x = (width_center + group_mean_to_center * Math.cos(degToRad(trial_index[3] * cuts))) + x.x * trial_index[2] * (group_range_unit));
        group_2_coord.map(x => x.y = height_center + group_mean_to_center * Math.sin(degToRad(trial_index[3] * cuts)) + x.y * trial_index[2] * (group_range_unit));

        all_coord = [group_1_coord].concat(group_2_coord);
    } else {
        group_1_coord.map(x => x.x = (width_center + group_mean_to_center * Math.cos(degToRad(180 + trial_index[3] * cuts))) + x.x * trial_index[2] * (group_range_unit));
        group_1_coord.map(x => x.y = height_center + group_mean_to_center * Math.sin(degToRad(180 + trial_index[3] * cuts)) + x.y * trial_index[2] * (group_range_unit));
        group_2_coord.map(x => x.x = (width_center + group_mean_to_center * Math.cos(degToRad(trial_index[3] * cuts))) + x.x * trial_index[2] * (group_range_unit));
        group_2_coord.map(x => x.y = height_center + group_mean_to_center * Math.sin(degToRad(trial_index[3] * cuts)) + x.y * trial_index[2] * (group_range_unit));
        all_coord = group_1_coord.concat(group_2_coord);
    }
    return coord = {
        group_1_coord: group_1_coord,
        group_2_coord: group_2_coord,
        // rand_index_1_list: rand_index_1_list,
        // rand_index_2_list: rand_index_2_list,
        all_coord: all_coord
    };
}
//buttons
var btn = document.getElementById("btn");

//axis & number input boxes
var img = new Image();
img.src = 'img/Coordinate.png';
img.onload = () => {
    ctx.drawImage(img, 0, 0, canvas.width, canvas.height);
};

var box_width = (canvas.width / 4) + "px";
var box_height = (canvas.height / 8) + "px";

left_offset = (innerWidth - canvas.width) / 2 - (canvas.width / 8);
top_offset = (innerHeight - canvas.height) / 2 - (canvas.height / 16);

// function addInput(x_1, y_1, x_2, y_2) {
//     var input_1 = document.createElement('input');
//     var input_2 = document.createElement('input');

//     input_1.id = "input_1";
//     input_2.id = "input_2"

//     document.body.appendChild(input_1);
//     document.body.appendChild(input_2);
//     input_1.type = 'number';
//     input_2.type = 'number';
//     input_1.style.position = 'fixed';
//     input_2.style.position = 'fixed';

//     input_1.style.left = left_offset + x_1 + 'px';
//     input_1.style.top = top_offset + y_1 + 'px';
//     input_2.style.left = left_offset + x_2 + 'px';
//     input_2.style.top = top_offset + y_2 + 'px';

//     input_1.style.width = box_width;
//     input_1.style.fontSize = box_height;
//     input_1.style.borderWidth = "10px";
//     input_1.style.textAlign = "center";

//     input_2.style.width = box_width;
//     input_2.style.fontSize = box_height;
//     input_2.style.borderWidth = "10px";
//     input_2.style.textAlign = "center";

//     btn.innerHTML = "SUBMIT";
//     btn.disabled = false;
// }

// // function handleEnter(e) {
// //     var keyCode = e.keyCode;
// //     if (keyCode === 13) {
// //     }

// // }

// function drawText(txt_1, txt_2, x_1, y_1, x_2, y_2) {
//     ctx.textBaseline = 'top';
//     ctx.textAlign = 'left';
//     ctx.fillText(txt_1, x_1, y_1);
//     ctx.fillText(txt_2, x_2, y_2);

// }
