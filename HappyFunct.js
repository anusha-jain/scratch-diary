var a=0
function Brightside(){
	var x = document.getElementById("pic");
	var y = document.getElementById("music");
	if (a==0){
		x.style.display="block";
		y.style.display="block";
		a++
	}else {
		x.style.display="none";
		y.style.display="none";
		a--
	}
	
}