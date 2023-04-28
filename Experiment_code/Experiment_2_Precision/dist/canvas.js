var client = parseClient();
//trial info
var trialNumber = 0;
var trialData = [];

//canvas parameters
var canvas = document.getElementById("canvas");
var ctx = canvas.getContext("2d");

const width_height = Math.min(innerWidth, innerHeight);
console.log(innerHeight);
canvas.width = innerHeight * 0.99;
canvas.height = innerHeight * 0.99;
const width_center = canvas.width / 2;
const height_center = canvas.height / 2;
console.log(canvas.width);

// sub_canvas
// sample the center of two groups from 2-D uniform 
// const sub_canvas_center_x_min = canvas.width * 5 / 11;
// const sub_canvas_center_x_max = canvas.width * 6 / 11;
// const sub_canvas_center_y_min = canvas.height * 5 / 11;
// const sub_canvas_center_y_max = canvas.height * 6 / 11;

// var sub_canvas_center_list = [];

// for (let i = 0; i < 660; i++) {
//     sub_canvas_center_list.push(sample_uniform_2d(
//         [sub_canvas_center_x_min, sub_canvas_center_x_max],
//         [sub_canvas_center_y_min, sub_canvas_center_y_max]));
// }


// circle stimuli size
var radius = canvas.height / 200;
var group_range_unit = canvas.height / 8;
var group_mean_to_center = canvas.height / 8 * 2.5;


var group_1_center_x;
var group_1_center_y;
var group_2_center_x;
var group_2_center_y;

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
// 16 conditions ([group_1_size, group_2_size, group_1_sd, group_2_sd])
size_sd_pairs = [
    [1, 4, 1, 2],
    [1, 4, 1, 1],
    [1, 4, 1, 0.5],
    [1, 16, 1, 2],
    [1, 16, 1, 1],
    [1, 16, 1, 0.5],
    [1, 64, 1, 2],
    [1, 64, 1, 1],
    [1, 64, 1, 0.5],
    [4, 4, 1, 2],
    [4, 4, 1, 1],
    [4, 4, 1, 0.5],
    [4, 4, 2, 0.5],
    [16, 16, 1, 2],
    [16, 16, 1, 1],
    [16, 16, 1, 0.5],
    [16, 16, 2, 0.5],
    [64, 64, 1, 2],
    [64, 64, 1, 1],
    [64, 64, 1, 0.5],
    [64, 64, 2, 0.5],
    [2, 4, 1, 1],
    [2, 32, 1, 1],
    [8, 32, 1, 1],
    [8, 64, 1, 1]
];

// Indexing
num_trials = 20;
var means_index = jStat.seq(0, num_trials - 1, num_trials);
var cuts = 360 / num_trials;

// Indexing rule: [group_1_size, group_2_size, range_multiplier, mean_index]

// Practice trial
// var practice_1 = [2, 4, 1, 1, 0];
// var practice_2 = [2, 16, 1, 1, -8];
// var practice_3 = [1, 16, 1, 1, -16];
// var practice_4 = [1, 64, 1, 0.5, -10];
// var practice_5 = [16, 16, 1, 2, -4];
// var practice_6 = [8, 64, 1, 1, -12];

var practice_1 = [1, 64, 1, 2, 0];
var practice_2 = [1, 64, 1, 1, 0];
var practice_3 = [1, 64, 1, 0.5, 0];
var practice_4 = [64, 64, 2, 0.5, 0];
var practice_5 = [16, 16, 1, 1, 0];
var practice_6 = [16, 16, 1, 0.5, 0];

// Trial structure: 6 practice trials for each part + 20*25 = 400 (experimental trials) for each part (total: 806)
var trial_size_sd_pairs = repeat_each_array(size_sd_pairs, num_trials);
var trial_means_index = repeat_whole_single(means_index, 25);
var trial_index = combineArray(trial_size_sd_pairs, trial_means_index);
// trial_index = addArray(trial_index, trial_means_index);

trial_index = shuffle(trial_index);

trial_index.splice(0, 0, practice_6);
trial_index.splice(0, 0, practice_5);
trial_index.splice(0, 0, practice_4);
trial_index.splice(0, 0, practice_3);
trial_index.splice(0, 0, practice_2);
trial_index.splice(0, 0, practice_1);


console.log(trial_index);

// trial_index = combineArray(trial_index, sub_canvas_center_list);


// samples
const samples = JSON.parse(all_samples);

//random index
const size_index_1 = jStat.seq(0, 99, 100);
const size_index_2 = jStat.seq(0, 99, 100);

// var rand_index_1;
// var rand_index_2;

// var rand_index_1_list = [];
// var rand_index_2_list = [];

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
        group_1_coord.x = width_center + group_mean_to_center * Math.cos(degToRad(180 + trial_index[4] * cuts));
        group_1_coord.y = height_center + group_mean_to_center * Math.sin(degToRad(180 + trial_index[4] * cuts));

        group_2_coord.x = width_center + group_mean_to_center * Math.cos(degToRad(trial_index[4] * cuts));
        group_2_coord.y = height_center + group_mean_to_center * Math.sin(degToRad(trial_index[4] * cuts));
        all_coord = [group_1_coord].concat([group_2_coord]);
    } else if (trial_index[0] != 1 && trial_index[1] == 1) {
        group_1_coord.map(x => x.x = (width_center + group_mean_to_center * Math.cos(degToRad(180 + trial_index[4] * cuts))) + x.x * trial_index[2] * (group_range_unit));
        group_1_coord.map(x => x.y = height_center + group_mean_to_center * Math.sin(degToRad(180 + trial_index[4] * cuts)) + x.y * trial_index[2] * (group_range_unit));

        group_2_coord.x = width_center + group_mean_to_center * Math.cos(degToRad(trial_index[4] * cuts));
        group_2_coord.y = height_center + group_mean_to_center * Math.sin(degToRad(trial_index[4] * cuts));

        all_coord = group_1_coord.concat([group_2_coord]);
    } else if (trial_index[0] == 1 && trial_index[1] != 1) {
        group_1_coord.x = width_center + group_mean_to_center * Math.cos(degToRad(180 + trial_index[4] * cuts));
        group_1_coord.y = height_center + group_mean_to_center * Math.sin(degToRad(180 + trial_index[4] * cuts));

        group_2_coord.map(x => x.x = (width_center + group_mean_to_center * Math.cos(degToRad(trial_index[4] * cuts))) + x.x * trial_index[3] * (group_range_unit));
        group_2_coord.map(x => x.y = height_center + group_mean_to_center * Math.sin(degToRad(trial_index[4] * cuts)) + x.y * trial_index[3] * (group_range_unit));

        all_coord = [group_1_coord].concat(group_2_coord);
    } else {
        group_1_coord.map(x => x.x = (width_center + group_mean_to_center * Math.cos(degToRad(180 + trial_index[4] * cuts))) + x.x * trial_index[2] * (group_range_unit));
        group_1_coord.map(x => x.y = height_center + group_mean_to_center * Math.sin(degToRad(180 + trial_index[4] * cuts)) + x.y * trial_index[2] * (group_range_unit));
        group_2_coord.map(x => x.x = (width_center + group_mean_to_center * Math.cos(degToRad(trial_index[4] * cuts))) + x.x * trial_index[3] * (group_range_unit));
        group_2_coord.map(x => x.y = height_center + group_mean_to_center * Math.sin(degToRad(trial_index[4] * cuts)) + x.y * trial_index[3] * (group_range_unit));
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


