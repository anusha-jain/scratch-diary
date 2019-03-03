const leafinfo = [];
leafinfo[0] = "Leaf 1";
leafinfo[1] = "Leaf 2";
leafinfo[2] = "Leaf 3";
leafinfo[3] = "Leaf 4";
leafinfo[4] = "Leaf 5";
leafinfo[5] = "Leaf 6";
leafinfo[6] = "Leaf 7";
leafinfo[7] = "Leaf 8";
leafinfo[8] = "Leaf 9";
leafinfo[9] = "Leaf 10";
leafinfo[10] = "Leaf 11";
leafinfo[11] = "Leaf 12";
leafinfo[12] = "Leaf 13";
leafinfo[13] = "Leaf 14";

function Score(){
	for (var i=0; i<20; i++) {
		var scr = [];
		scr[i] = Math.floor(Math.random() *3);
		var leaf = [];
		leaf[i] = document.getElementsByClassName("leaf")[i];
		switch(scr[i]){
			case 0:
			leaf[i].setAttribute("stroke","Brown");
			leaf[i].setAttribute("fill","Brown");
			break;
			
			case 1:
			leaf[i].setAttribute("stroke","Yellow");
			leaf[i].setAttribute("fill","Yellow");
			break;
			
			case 2:
			leaf[i].setAttribute("stroke","MediumSeaGreen");
			leaf[i].setAttribute("fill","MediumSeaGreen");
			break;
		}
	}
}

document.addEventListener("click",Stuff);

var a = 0;
function Stuff (event){
	if(event.target.getAttribute("class")=="leaf"){
		number = parseInt(event.target.getAttribute("id"))
		idk = leafinfo[number];
		if(a==0){
			document.getElementById("info").setAttribute("fill","MediumSeaGreen");
			document.getElementById("info").setAttribute("stroke","MediumSeaGreen");
			document.getElementById("deets").setAttribute("fill","black");
			document.getElementById("deets").innerHTML=idk;
			a++;
		} else {
			document.getElementById("info").setAttribute("fill","none");
			document.getElementById("info").setAttribute("stroke","none");
			document.getElementById("deets").setAttribute("fill","none");
			document.getElementById("deets").innerHTML="Waiting";
			a--;
		}
	}
}