var cnv = document.getElementById('game');
cnv.width = window.innerWidth - 50;
cnv.height = window.innerHeight - 50;

function squared_distance(x1, y1, x2, y2) {
    var dx = x2 - x1;
    var dy = y2 - y1;
    return dx * dx + dy * dy;
}

var ctx = cnv.getContext('2d');
ctx.font = '20pt Monaco';

var v = 4;

var chars_img = new Image();
chars_img.src = 'sprites.png';
chars_img.alt = '???';

var ptrn;
var backimg = new Image();
backimg.src = 'grass1.jpg';
backimg.onload = function () {
    ptrn = ctx.createPattern(backimg, 'repeat');
};

var background = {
    refresh: function () {
        ctx.fillStyle = ptrn;
        ctx.fillRect(0, 0, cnv.width, cnv.height);
    }
};

function Character(ind) {
    this.x = 0; this.y = 0;
    this.dx = 0; this.dy = 0;
    this.dstW = 95; this.dstH = 120;
    this.life_left = 200;

    this.mutate(ind);
    this.newDirection();
};

Character.prototype.newDirection = function () {
    this.toX = Math.floor(cnv.width * Math.random());
    this.toY = Math.floor(cnv.height * Math.random());
    this.dx = 2 * (this.toX - this.x) / 80;
    this.dy = 2 * (this.toY - this.y) / 80;
};
Character.prototype.draw = function () {
    ctx.drawImage(chars_img, 
            this.srcX, this.srcY, this.dstW, this.dstH,
            this.x, this.y, this.dstW, this.dstH);
    //ctx.fillText('' + this.life_left, this.x, this.y);
};
Character.prototype.clear = function () {
    ctx.clearRect(this.x, this.y, this.dstW, this.dstH);
};
Character.prototype.mutate = function (ind) {
    this.srcX = (ind % 6) * this.dstW;
    this.srcY = Math.floor(ind / 6) * this.dstH;
    this.ind = ind;
};
Character.prototype.die = function () {
    this.mutate(16);
    this.dx = 0; this.dy = 0;
};
Character.prototype.move = function () {
    if (squared_distance(this.x, this.y, this.toX, this.toY) < 20) {
        this.newDirection();
    } else {
        this.x += this.dx;
        this.y += this.dy;
    }
    this.life_left -= 1;
    if (this.life_left < 1) {
        this.die();
    }
}

var me = new Character(7);
me.score = 4;
me.x = cnv.width/2;
me.y = cnv.height/2;
me.dx = me.dy = 0;
me.heal_ticks = -1;
me.timer = function () {
    this.x += this.dx;
    this.y += this.dy;
    if (me.heal_ticks < 0)
        return;
    me.heal_ticks -= 1;
    if (me.heal_ticks == 0) {
        me.heal_ticks = -1;
        me.mutate(7);
    }
};
me.bitten = function () {
    me.mutate(18); 
    me.heal_ticks = 200;
    --me.score;
};

var bugs = [ new Character(22), new Character(22) ];

function idlefunc() {
    background.refresh();

    me.timer();

    //me.move();
    me.draw();

    for (var i = 0; i < bugs.length; ++i) {
        var bug = bugs[i];
        if (bug.life_left < -200) continue;
        bug.move();

        if ((bug.ind = 22) && (me.ind == 7)
            && squared_distance(bug.x, bug.y, me.x, me.y) < 30 * 30)
        {
            //bug.dx = 0; bug.dy = 0;
            bugs[ bugs.length ] = new Character(22);
            me.bitten();
        }
        bug.draw();
    }

    ctx.fillText('Score: ' + me.score, cnv.width - 100, 10);

    if (Math.floor(Math.random() * 40) == 1)
        bugs[ bugs.length ] = new Character(22);
}

function keyDownHandler(event) {
    if (event.keyCode == 37) me.dx = -v;
    if (event.keyCode == 38) me.dy = -v;
    if (event.keyCode == 39) me.dx = v;
    if (event.keyCode == 40) me.dy = v;
}
function keyUpHandler(event) {
    if (event.keyCode == 37) me.dx = 0;
    if (event.keyCode == 38) me.dy = 0;
    if (event.keyCode == 39) me.dx = 0;
    if (event.keyCode == 40) me.dy = 0;
}

document.addEventListener('keydown', keyDownHandler);
document.addEventListener('keyup', keyUpHandler);
window.addEventListener('resize', function (event) {
    cnv.width = window.innerWidth - 30;
    cnv.height = window.innerHeight - 30;
});

setInterval(idlefunc, 50);
