
var slideIndex2 = 1;
showSlides2(slideIndex2);

function plusSlides2(n) {
//  slideIndex2 = slideIndex2 + n;
//   showSlides2(slideIndex2); 
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