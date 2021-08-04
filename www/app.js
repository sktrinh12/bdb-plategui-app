  $(document).ready(function() {
    // Enable button
      Shiny.addCustomMessageHandler("enableButton", function(id) {
    $("#" + id).removeAttr("disabled");
    });
    // Disable button
      Shiny.addCustomMessageHandler("disableButton", function(id) {
    $("#" + id).attr("disabled", "true");
    });
    // add class to loading
    Shiny.addCustomMessageHandler("addClass", function(data) {
      if (!$(`.${data.id}`).length) {
        console.log(`adding class for ${data.id}`);
        $(`#${data.btn}`).append(`<div class="${data.id}"></div>`);
      }
    });
    // remove class to loading
    Shiny.addCustomMessageHandler("removeClass", function(btn) {
      $(`#${btn}`).children('div').each(function () {
        this.remove();
        console.log(`removing class for ${this}`);
      })
    });
    // msgbox for push button
    Shiny.addCustomMessageHandler("msgbox", function(message) {
      alert(JSON.stringify(message));
    });
    // stop button disappear
    Shiny.addCustomMessageHandler("btnDisappear", function(id) {
      $("#" + id).fadeOut("slow");
    });
    // stop button appear
    Shiny.addCustomMessageHandler("btnAppear", function(id) {
      $("#" + id).show();
    });
    // save button load
    Shiny.addCustomMessageHandler("saveBtnMessage", function(message) {
      $("#saved_final").text(`STATUS: ${message}`);
    });
  })
