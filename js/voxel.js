var c = document.getElementById("Canvas");
c.width = document.body.clientWidth;
c.height = document.body.clientHeight;

var ctx = c.getContext("2d");
var world_size;
var vision = 900;
var physics_distance = 1200;

var fps = 30;
var time = 0;
var colors = [
    "#000000",//black
    "#008800",//green
    "#88FF00",//lime
    "#000088",//blue
    "#FF0000",//bright red
    "#FF0088",//pink
    "#880000",//red
    "#FF8800",//neon orange
    "#00FF88",//green3
];


function clockwise(p1, p2, p3) {
    //checking the sign of the cross product of a couple vectors made from these points.
    var v1 = [(p2.x - p1.x), (p2.y - p1.y)];
    var v2 = [(p3.x - p1.x), (p3.y - p1.y)];
    var cross = (v1[0]*v2[1]) - (v2[0]*v1[1]);
    return cross > 0;
};
function draw_triangle(p1, p2, p3, color) {
    //console.log("draw triangle");
    //console.log(JSON.stringify([p1, p2, p3, color]));
    var b = clockwise(p1, p2, p3);
    if (b) {
        ctx.beginPath();
        ctx.moveTo(p1.x, p1.y);
        ctx.lineTo(p2.x, p2.y);
        ctx.lineTo(p3.x, p3.y);
        ctx.fillStyle = color;
        ctx.fill();
    };
};
function draw_triangles(corners, Tris) {
    //console.log("draw triangles");
    for(i=0;i<Tris.length;i++){
    //console.log("draw triangle");
        var tri = Tris[i];
        var P0 = corners[tri[0]];
        var P1 = corners[tri[1]];
        var P2 = corners[tri[2]];
        //var f = function(Z) {return ((Z.z > 0) && (Z.z < vision) && (Z.x > -(vision/2)) && (Z.x < (vision/2)));};
        if(visible(P0) && visible(P1) && visible(P2)) {
            //console.log("draw triangle 2");
            //if ((P0.z > 0) && (P1.z > 0) && (P2.z > 0)) {
            var p1 = three_to_two(P0);
            var p2 = three_to_two(P1);
            var p3 = three_to_two(P2);
            draw_triangle(p1, p2, p3, tri[3]);
        };
    };
};
function average_point(p1, p2, p3) {
    return({x: (p1.x + p2.x + p3.x)/3,
            y: (p1.y + p2.y + p3.y)/3,
            z: (p1.z + p2.z + p3.z)/3})
};
function draw_helper(points2, tris) {
    var f = function(x) {
        var p2 = average_point(points2[x[0]],points2[x[1]],points2[x[2]]);
        var mapSize = world_size * 100;
        return(distance({x:((mapSize/2)),y:0,z:((mapSize/2))}, p2));
    };
    var triangles2 = tris.sort(function(t1, t2){return(f(t2) - f(t1))});
    ctx.clearRect(0, 0, c.width, c.height);
    draw_triangles(points2, triangles2);
};
function three_to_two(a) {
    var W = c.width;
    var Z = W;
    var H = c.height;
    var f = a.z / Z;
    var X = (W/2) + (a.x / f);
    var Y = (H/2) + (a.y / f);
    return {x: X, y: Y};
};
function visible(Z) {
    return ((Z.z > 0) && (Z.z < vision) && (Z.x > -(vision/2)) && (Z.x < (vision/2)));
};
function pdb_maker() {
    var db = {type: "points"};
    var top = 0;
    function add_point_inner(p) {
        db[top] = p;
        top += 1;
        return(top-1);
    };
    return({
        top: top,
        db: db,
        add_point: add_point_inner,
        add: function(x,y,z) {
            //db[top] = make_3_point(x, y, z);
            var point = {x: x, y: y, z: z};
            return(add_point_inner(point));
            //add_point_inner(point);
            //db[top] = {x: x, y: y, z: z};
            //top += 1;
            //return(top-1);
        },
        update: function(n,x,y,z) {
            //db[n] = make_3_point(x, y, z);
            db[n] = {x: x, y: y, z: z};
        },
        adjust: function(n,x,y,z) {
            var old = db[n];
            //db[n] = make_3_point(old.x + x, old.y + y, old.z + z);
            db[n] = {x: old.x + x, y: old.y + y, z: old.z + z};
        },
        perspective: function(){
            //rotates and shifts each point over based on your current location and the direction you are facing. Points are still specified in 3d.
            var r = rotation_matrix_y(perspective.theta);
            //console.log(r);
            r = mul_m_m(rotation_matrix_x(Math.PI/6), r);
            //console.log(JSON.stringify([r, r2]));
            var db2 = {};
            for(i=0;i<top;i++) {
                db2[i] = in_perspective(db[i], r);
            };
            return(db2);
        }
    });
};
function pos_mod(A, B) {
    return((((A % B) + B) % B) - (B/2));
}
function in_perspective(point, rotation) {
    var T = perspective.theta;
    var mapSize = world_size * 100;
    //var X = point.x - perspective.x;
    var X = point.x - (perspective.x + (100*Math.sin(T)));
    X = pos_mod(X, mapSize);
    var Y = point.y - (perspective.y - 100);
    var Z = point.z - (perspective.z - (100*Math.cos(T)));
    Z = pos_mod(Z, mapSize);
    var point2 = {x: X, y: Y, z: Z};
    var point3 = mul_v_m(point2, rotation);
    return(point3);
};
function rotation_matrix_y(angle) {
    return([
        [Math.cos(angle),0,Math.sin(angle)],
        [0,1,0],
        [-Math.sin(angle),0,Math.cos(angle)]]);
};
function rotation_matrix_x(angle) {
    return([[1,0,0],
            [0,Math.cos(angle),-Math.sin(angle)],
            [0,Math.sin(angle),Math.cos(angle)]]);
};
function mul_v_v(p, v) {
    return (p.x*v[0]) + (p.y * v[1]) + (p.z * v[2]);
};
function mul_v_m(p, m){
    var p2 = JSON.parse(JSON.stringify(p));
    p2.x = mul_v_v(p, m[0]);
    p2.y = mul_v_v(p, m[1]);
    p2.z = mul_v_v(p, m[2]);
    return(p2);
};
function mul_m_m(m1, m2){

};

function column(m, n) {//diagonal flip
    return(m.map(function(x){return(x[n])}));
};
function v2p(v) {
    return({x:v[0],y:v[1],z:v[2]});
};

function mul_m_m(m1, m2) {
    var cs = [0,1,2].map(function(x){return(column(m2, x))});
    return([0,1,2].map(function(n){return(
        cs.map(function(x){return(
            mul_v_v(v2p(m1[n]), x))}))}));
};

function cube_points(p, s, pdb) {
    var X = p.x;
    var Y = p.y;
    var Z = p.z;
    var L = [p,
             {x: X+s, y: Y, z: Z, color: p.color},
             {x: X, y: Y+s, z: Z, color: p.color},
             {x: X, y: Y, z: Z+s, color: p.color},
             {x: X+s, y: Y+s, z: Z, color: p.color},
             {x: X+s, y: Y, z: Z+s, color: p.color},
             {x: X, y: Y+s, z: Z+s, color: p.color},
             {x: X+s, y: Y+s, z: Z+s, color: p.color},
            ];
    return(L.map(function(p){return(pdb.add_point(p));}));
};
function cp2faces(pts) {
    return([[pts[0],pts[1],pts[2],pts[4]],
            [pts[5],pts[3],pts[7],pts[6]],
            [pts[0],pts[2],pts[3],pts[6]],
            [pts[4],pts[1],pts[7],pts[5]],
            [pts[1],pts[0],pts[5],pts[3]],
            [pts[2],pts[4],pts[6],pts[7]],
           ])
};
function face2triangles(face, color,pdb){
    var c = pdb.db[face[0]].color;
    return([[face[0],face[1],face[2],c],
            [face[1],face[3],face[2],c]]);
};

function distance(p1, p2) {
    var mapSize = world_size * 100;
    var xd = pos_mod((p1.x - p2.x), mapSize);
    var yd = (p1.y - p2.y) % mapSize;
    var zd = pos_mod((p1.z - p2.z), mapSize);
    return(Math.sqrt((xd*xd)+(yd*yd)+(zd*zd)))
};

function surrounded(X,Y,Z,W) {
    //if all 6 spots surrounding a location are non-empty, then we don't need to bother drawing that cube.
    var x = X;
    var y = Y;
    var z = Z;
    if(x == (-1)){
        x += W.length;
    };
    if(y == (-1)){
        y += W.length;
    };
    if(z == (-1)){
        z += W.length;
    };
    var x2 = x-1;
    if(x2 == (-1)){
        x2 += W.length;
    };
    var y2 = y-1;
    if(y2 == (-1)){
        y2 += W.length;
    };
    var z2 = z-1;
    if(z2 == (-1)){
        z2 += W.length;
    };
    var x3 = x+1;
    if(x3 >= W.length){
        x3 -= W.length
    };
    var y3 = y+1;
    if(y3 >= W.length){
        y3 -= W.length
    };
    var z3 = z+1;
    if(z3 >= W.length){
        z3 -= W.length
    };
    if(W[x3][y][z]==0){
        return(false);
    };
    if(W[x2][y][z]==0){
        return(false);
    };
    if(W[x][y3][z]==0){
        return(false);
    };
    if(W[x][y2][z]==0){
        return(false);
    };
    if(W[x][y][z3]==0){
        return(false);
    };
    if(W[x][y][z2]==0){
        return(false);
    };
    return(true);
};

function grid_to_points2(W, X, Y, L, F) {
    if(X == W.length){
        return(F(L));
    };
    if(Y == W.length){
        return(grid_to_points2(W, X+1, 0, L, F));
    };
    for(var z=0;z<W.length;z++){
        var g = W[X][Y][z];
        if(g == 0){
        } else if(surrounded(X,Y,z,W)){
        } else {
            L = L.concat([{x: X*100,
                           y: Y*100,
                           z: z*100,
                           color: colors[g]}]);
        };
    };
    if((Y % Math.round(W.length/3)) == 0){
        setTimeout(function(){
            return(grid_to_points2(W, X, Y+1, L, F));
        }, 0);
    } else{
        return(grid_to_points2(W, X, Y+1, L, F));
    };
};
    
function main(){
    var round = 0;
    var pdb = pdb_maker();
    var triangles = [];
    function cron(){
        if((round % fps) == 0) {
            grid_to_points2(cube_grid,0,0,[],function(cps){
                var cps2 = cps.filter(function(p){
                    return(distance(p,perspective)<physics_distance);
                });
                var pdb2 = pdb_maker();
                var faces = cps2.reduce(function(a, x){return(cp2faces(cube_points(x, 100, pdb2)).concat(a))}, []);
                var triangles2 = faces.reduce(function(a,x){return(a.concat(face2triangles(x,colors[3],pdb2)));}, []);
                pdb = pdb2;
                triangles = triangles2;
            });
        };
        round += 1;
        movement([13,16,37,38,39,40,65,83]);
        var pdb2 = pdb.perspective();
        draw_helper(pdb2, triangles);
        setTimeout(cron, 1000/fps);
    };
    cron();
};

//setTimeout(function(){
//    main();
//}, 100);

function update_map(F){
    variable_public_get(["read"],function(b){
        var v = atob(b);
        var c = 0;
        var a = [];
        var S = Math.cbrt(v.length);
        world_size = S;
        for(var x=0;x<S;x++){
            a[x] = [];
            for(var y=0;y<S;y++){
                a[x][y] = [];
                for(var z=0;z<S;z++){
                    a[x][y][z] = v[c].charCodeAt();
                    c++
                };
            };
        };
        //console.log(JSON.stringify(a));
        F(a);
    });
};
//var cps = [];

var cube_grid;


function map_cron(){
    update_map(function(W){
        cube_grid = W;
        return(0);
    });
    setTimeout(map_cron, 3000);
}
map_cron();
setTimeout(main, 1500);





//Controller

function cursor() {
    var X = perspective.x;
    var Y = perspective.y;
    var Z = perspective.z;
    var T = perspective.theta;
    var mapSize = world_size * 100;
    X = pos_mod(X, mapSize);
    Z = pos_mod(Z, mapSize);
    X -= 50;
    Y -= 50;
    Z -= 50;
    X /= 100;
    Y /= 100;
    Z /= 100;
    Z += Math.cos(T);
    X -= Math.sin(T);
    X = X+world_size;
    Z = Z+world_size;
    X = X % world_size;
    Z = Z % world_size;
    return([X, Y, Z]);
};

var controls = {
    13:false,
    16:false,
    37:false,
    38:false,
    39:false,
    40:false,
    65:false,
    83:false};
var perspective = {x:50,y:50,z:50,theta:0};
var bag = [];

var step_size = 100;
var turn_speed = 0.06;
function left() {
    //perspective.theta += turn_speed;
    perspective.theta += Math.PI/2;
    perspective.theta %= (2*Math.PI);
};
function step_left() {
    var T = perspective.theta;
    var S = Math.sin(T);
    var C = Math.cos(T);
    perspective.z -= Math.round(S*step_size);
    perspective.x -= Math.round(C*step_size);
};
function up() {
    var T = perspective.theta;
    var S = Math.sin(T);
    var C = Math.cos(T);
    perspective.z += Math.round((C*step_size));
    perspective.x -= Math.round((S*step_size));
};
function right() {
    perspective.theta -= Math.PI/2;
    perspective.theta %= (2*Math.PI);
};
function step_right() {
    var T = perspective.theta;
    var S = Math.sin(T);
    var C = Math.cos(T);
    perspective.z += Math.round(S*step_size);
    perspective.x += Math.round(C*step_size);
};
function down() {
    var T = perspective.theta;
    var S = Math.sin(T);
    var C = Math.cos(T);
    perspective.z -= Math.round(C*step_size);
    perspective.x += Math.round(S*step_size);
};
function give() {
    console.log("give");
    console.log(JSON.stringify(cursor()));
    var C = cursor();
    var C1d = (C[1]+1) % world_size;
    var C1u = ((C[1]-1)+world_size) % world_size;

    var Mid = cube_grid[C[0]][C[1]][C[2]];
    var Up = cube_grid[C[0]][C1u][C[2]];
    var Down = cube_grid[C[0]][C1d][C[2]];
    if(Down == 0) {
        variable_public_get(["add",C[0],C1d,C[2],bag[0]],function(x){
            return(0);
        });
        cube_grid[C[0]][C1d][C[2]] = bag[0];
    }else if(Mid == 0) {
        variable_public_get(["add",C[0],C[1],C[2],bag[0]],function(x){
            return(0);
        });
        cube_grid[C[0]][C[1]][C[2]] = bag[0];
    }else if(Up == 0) {
        variable_public_get(["add",C[0],C1u,C[2],bag[0]],function(x){
            return(0);
        });
        cube_grid[C[0]][C1u][C[2]] = bag[0];
    };
    bag = bag.slice(1);
};
function eat() {
    console.log("eat");
    console.log(JSON.stringify(cursor()));
    var C = cursor();
    var C1d = (C[1]+1) % world_size;
    var C1u = ((C[1]-1)+world_size) % world_size;

    var Mid = cube_grid[C[0]][C[1]][C[2]];
    var Up = cube_grid[C[0]][C1u][C[2]];
    var Down = cube_grid[C[0]][C1d][C[2]];

    if(!(Mid == 0)) {
        variable_public_get(["take",C[0],C[1],C[2]],function(x){
            return(0);
        });
        bag = ([Mid]).concat(bag);
        cube_grid[C[0]][C[1]][C[2]] = 0;
    }else if(!(Up == 0)) {
        variable_public_get(["take",C[0],C1u,C[2]],function(x){
            return(0);
        });
        bag = ([Up]).concat(bag);
        cube_grid[C[0]][C1u][C[2]] = 0;
    }else if(!(Down == 0)) {
        variable_public_get(["take",C[0],C1d,C[2]],function(x){
            return(0);
        });
        bag = ([Down]).concat(bag);
        cube_grid[C[0]][C1d][C[2]] = 0;
    }
    //bag is a stack 
};

var keys = {};
keys[13] = give;//enter key gives
keys[16] = eat;//shift key eats
keys[37] = left;
keys[38] = up;
keys[39] = right;
keys[40] = down;
keys[65] = step_left;
keys[83] = step_right;
document.addEventListener('keydown', function(event) {
    var k = event.keyCode;
    //console.log(k);
    var cv = controls[k];
    if(cv == false) {
        controls[k] = true;
    };

    //var f = keys[event.keyCode];
    //if(!(f == undefined)){ f(); };
//    console.log(event.keyCode);
});
/*
document.addEventListener('keyup', function(event) {
    var k = event.keyCode;
    var cv = controls[k];
    if(cv == true) {
        controls[k] = false;
    };
});
*/ 
function movement(L){
    if(L.length == 0){return(0);};
    var H = L[0];
    //console.log(H);
    //console.log(JSON.stringify(controls));
    if(controls[H]){
        keys[H]();
        controls[H] = false;
    };
    return(movement(L.slice(1)));
};




