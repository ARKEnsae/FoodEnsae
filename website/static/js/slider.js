
var slideIndex2 = 1;
showSlides2(slideIndex2);

function plusSlides2(n) {
  showSlides2(slideIndex2 += n);
}

function currentSlide2(n) {
  showSlides2(slideIndex2 = n);
}

function showSlides2(n) {
  var i;
  var slides = document.getElementsByClassName("element");
  if (n > slides.length) {slideIndex2 = 1}
  if (n < 1) {slideIndex2 = slides.length}
  for (i = 0; i < slides.length; i++) {
      slides[i].style.display = "none";
     // slides[i].style.display = "block";
      
  }
  slides[slideIndex2-1].style.display = "block";
  //slides[slideIndex2-1].className += " active"; //NEW
} 


//Automatiser le diaporama (not working)

function startTimer(){
    timer = setInterval(function () {
        plusSlides2(1);
    }, 10000);
}
function stopTimer(){
    clearInterval(timer);
}

timer = setInterval(function () {
    plusSlides2(1);
}, 10000);
diapo.addEventListener("mouseover", stopTimer);
diapo.addEventListener("mouseout", startTimer);


