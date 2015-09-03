var gl;
var width = 1000, height = 500;
var view_angle = 75,aspect = width/height, near = 0.1, far = 1000;
var mouseX,mouseY;
var scene,camera,renderer;
function updatestate(sometext){
    var x = document.getElementsByClassName("headtext");
    for (u=0;u<x.length;u++) 
    {
        x[u].innerHTML = sometext; 
    }
    console.log(sometext);
}
function start() {
    updatestate("js start");
    var canvas = $("#glcanvas");
    
    gl = initWebGL(canvas);      // the GL context
}
function onDocumentMouseMove(event){
   mouseX = event.clientX - width/2;
   mouseY = -(event.clientY - height/2);
}
function keypress(e){
    console.log(e.keyCode);
    if (e.keyCode == "81"){
        camerachange(0,0,-1);
    } else if (e.keyCode == "69"){
        camerachange(0,0,1);
    }else if (e.keyCode == "87"){
        camerachange(0,-1,0);
    }else if (e.keyCode == "83"){
        camerachange(0,1,0);
    }else if (e.keyCode == "65"){
        camerachange(1,0,0);
    }else if (e.keyCode == "68"){
        camerachange(-1,0,0);
    }else if (e.keyCode == "40"){
        camera.translateY(-1);
    }else if (e.keyCode == "38"){
        camera.translateY(1);
    }else if (e.keyCode == "37"){
        camera.translateX(-1);
    }else if (e.keyCode == "39"){
        camera.translateX(1);
    }else if (e.keyCode == "79"){
        camera.translateZ(-1);
    }else if (e.keyCode == "80"){
        camera.translateZ(1);
    }else if (e.keyCode == "86"){
        console.log(camera.position);
    }else if (e.keyCode == "32"){
        console.log(mouseX,mouseY);
        alert(mouseY);
    }


}

function camerachange(du,dv,dw){
    var t=0.05
    camera.rotateOnAxis(new THREE.Vector3(-dv, du, -dw),t);

}
function render(){
    requestAnimationFrame(render);
    //camerachange(mouseX/100,mouseY/100); 
    renderer.render(scene,camera);
}
function addline(pt1,pt2,f){
    geo = new THREE.Geometry();
    geo.vertices.push(new THREE.Vector3(pt1[0]*f,pt1[1]*f,pt1[2]*f), new THREE.Vector3(pt2[0]*f,pt2[1]*f,pt2[2]*f));
    var line = new THREE.Line(geo,new THREE.LineBasicMaterial({color:0x00ff00}));
    scene.add(line);
}

function initWebGL(canvas){
    renderer = new THREE.WebGLRenderer({canvas: canvas.get(0)});
    renderer.setSize(width,height);
    camera = new THREE.PerspectiveCamera(view_angle,aspect,near,far);
    camera.position.set(0,0,0);
    scene = new THREE.Scene();
    updatestate("scene set");
    renderer.setClearColor(0x333F47, 1);
    //var light = new THREE.PointLight(0xffffff);
    //light.position.set(-100,200,100);
    //scene.add(light);
    $.getJSON("./test.json", function(longlist) {
           console.log(longlist.length);
           longlist.forEach(function(edge){
               addline(edge[0],edge[1],10);
               console.log(edge[0]);
           });
    });

    var geometry = new THREE.BoxGeometry( 10, 10, 10 );
    var material = new THREE.MeshBasicMaterial( {color: 0xff0000,wireframe: true} );
    //var cube = new THREE.Mesh( geometry, material );
    //cube.position.set(5,5,5);
    //scene.add( cube );
    //updatestate("cubeadded");
    document.addEventListener("mousemove",onDocumentMouseMove,false);
    document.addEventListener("keydown",keypress,false);
    updatestate("rendering"); 
    render();
    return renderer.context;
}
