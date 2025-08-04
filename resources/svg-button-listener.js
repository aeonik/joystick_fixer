  <script type="text/javascript"><![CDATA[
  document.addEventListener("DOMContentLoaded", function() {
    console.log("SVG DOM ready!");

    const buttons = document.querySelectorAll('.button-box');

    buttons.forEach(function(button) {
      button.style.cursor = 'pointer';
      button.addEventListener('click', function() {
        const message = "clicked:" + button.id;
        console.log(message);
        window.status = message;
      });
    });
  });
]]></script>
