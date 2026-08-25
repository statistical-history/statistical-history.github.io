document.addEventListener("DOMContentLoaded", function () {
  var menus = document.querySelectorAll(".site-nav__menu");

  menus.forEach(function (menu) {
    menu.addEventListener("keydown", function (event) {
      if (event.key === "Escape" && menu.open) {
        menu.open = false;
        menu.querySelector("summary").focus();
      }
    });

    menu.querySelectorAll(".site-nav__dropdown a").forEach(function (link) {
      link.addEventListener("click", function () {
        menu.open = false;
      });
    });
  });

  document.addEventListener("click", function (event) {
    menus.forEach(function (menu) {
      if (menu.open && !menu.contains(event.target)) {
        menu.open = false;
      }
    });
  });
});
