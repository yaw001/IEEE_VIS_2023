function circle(x, y, radius, color) {
    ctx.beginPath();
    ctx.arc(x, y, radius, 0, Math.PI * 2, false);
    ctx.fillStyle = color;
    ctx.fill();
    ctx.closePath();
}

function response_circle(x, y, radius, color) {
    clear();
    ctx.beginPath();
    ctx.arc(x, y, radius, 0, Math.PI * 2, false);
    ctx.fillStyle = color;
    ctx.fill();
    ctx.closePath();
    ctx.globalAlpha = 0.8;
}

function triangle(x, y, radius, color) {
    ctx.beginPath();
    ctx.moveTo(x, y - radius * 1.3);
    ctx.lineTo(x + radius * 1.3 * Math.cos(22.5 / 180 * Math.PI), y + radius * 1.3 * Math.sin(22.5 / 180 * Math.PI));
    ctx.lineTo(x - radius * 1.3 * Math.cos(22.5 / 180 * Math.PI), y + radius * 1.3 * Math.sin(22.5 / 180 * Math.PI));
    ctx.fillStyle = color;
    ctx.closePath();
    ctx.fill();
}

function rectangle(x, y, radius, color) {
    ctx.beginPath();
    ctx.rect(x - radius, y - radius, 2 * radius, 2 * radius);
    ctx.fillStyle = color;
    ctx.fill();
}

function cross(x, y, radius, color) {
    ctx.beginPath();
    ctx.moveTo(x + radius*1.5 * Math.cos(45 / 180 * Math.PI), y + radius*1.5 * Math.sin(45 / 180 * Math.PI));
    ctx.lineTo(x - radius*1.5 * Math.cos(45 / 180 * Math.PI), y - radius*1.5 * Math.sin(45 / 180 * Math.PI));
    ctx.lineTo(x, y);
    ctx.lineTo(x + radius*1.5 * Math.cos(45 / 180 * Math.PI), y - radius*1.5 * Math.sin(45 / 180 * Math.PI));
    ctx.lineTo(x - radius*1.5 * Math.cos(45 / 180 * Math.PI), y + radius*1.5 * Math.sin(45 / 180 * Math.PI));
    ctx.strokeStyle = color;
    ctx.lineWidth = 5;
    ctx.stroke();
}

function response_cross(x, y, radius, color) {
    clear();
    ctx.beginPath();
    ctx.moveTo(x + radius*1.5 * Math.cos(45 / 180 * Math.PI), y + radius*1.5 * Math.sin(45 / 180 * Math.PI));
    ctx.lineTo(x - radius*1.5 * Math.cos(45 / 180 * Math.PI), y - radius*1.5 * Math.sin(45 / 180 * Math.PI));
    ctx.lineTo(x, y);
    ctx.lineTo(x + radius*1.5 * Math.cos(45 / 180 * Math.PI), y - radius*1.5 * Math.sin(45 / 180 * Math.PI));
    ctx.lineTo(x - radius*1.5 * Math.cos(45 / 180 * Math.PI), y + radius*1.5 * Math.sin(45 / 180 * Math.PI));
    ctx.strokeStyle = color;
    ctx.lineWidth = 5;
    ctx.stroke();
    ctx.globalAlpha = 0.8;
}

function clear() {
    ctx.clearRect(canvas.width * 0.05, 0, canvas.width*0.95, canvas.height*0.95);
}
