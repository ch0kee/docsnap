//ajax szívdobogás
function heartbeat()
{
	loadContent();
/*	
	var editorBox = document.getElementById("idEditorBox");
	var s = editorBox.value;
	var r = new XMLHttpRequest();
	//var rJSON = {"content": editorBox.value}

	
	r.onreadystatechange=function()
	{
		if (r.readyState == 4 && r.status == 200)
		{
			editorBox.value = r.responseText;
		}
	}
	
	r.open("GET", "cupdate?c=" + s, true); //async?yeah = ajax
	r.send();
	*/
}


//érték mentése
function storeContent()
{
	var editorBox = document.getElementById("idEditorBox");
	var s = editorBox.value;
	var r = new XMLHttpRequest();
	//var rJSON = {"content": editorBox.value}

	
	r.onreadystatechange=function()
	{
		if (r.readyState == 4 && r.status == 200)
		{
			editorBox.value = r.responseText;
		}
	}
	
	r.open("GET", "cupdate?c=" + s, true); //async?yeah = ajax
	r.send();
}

//csak lekérjük az értéket
function loadContent()
{
	var editorBox = document.getElementById("idEditorBox");
	var r = new XMLHttpRequest();
	
	r.onreadystatechange=function()
	{
		if (r.readyState == 4 && r.status == 200)
		{
			editorBox.value = r.responseText;
		}
	}
	
	r.open("GET", "cupdate", true); //async?yeah = ajax
	r.send();
}

window.onload = loadContent;
var ajaxHeartBeat = setInterval(heartbeat, 1000);