// experiment settings
var expt = {
    name: 'Ensmeble perception',
    maxTrials: 406,
    saveURL: 'submit.simple.php',
    // sona: {
    //     experiment_id: 2224,
    //     credit_token: '77c344ea31a9488f8e1b7e20ec8f16c2'
    // }
};

function debugLog(message) {
    if (expt.debug) {
        console.log(message);
    }
}