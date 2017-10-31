//Attention: ne pas mettre de caracteres non ASCII dans ce fichier

Shiny.addCustomMessageHandler("msgboxError", function(message) {alert(message);});

$(document).on('shiny:value', function(event) {
  if (event.target.id === 'map') {
	//Remarque: il faut envoyer une valeur difference a chaque passage sinon l evt n est pas detecte cote serveur
	Shiny.onInputChange("carteAffichee", new Date().valueOf());
  }
});