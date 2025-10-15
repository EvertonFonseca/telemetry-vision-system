
# app/model/Swiper.R
box::use(
  htmltools,
  shiny[tags],
  shinyjs,
  ../view/global[moveScrollToUp]
)

# Dependência do Swiper (CDN)
# - usa a estrutura padrão da CDN: .../css/... e .../js/...
# - htmlDependency com 'src = c(href=...)' para montar URLs relativos
#' @export
swiperDep <- function() {
  htmltools$htmlDependency(
    name = "swiper",
    version = "4.4.6",
    src = c(href = "https://cdnjs.cloudflare.com/ajax/libs/Swiper/4.4.6"),
    script = "js/swiper.min.js",
    stylesheet = "css/swiper.min.css"
  )
}

# Slide wrapper
#' @export
swiperSlide <- function(...) {
  htmltools$tags$div(class = "swiper-slide", ...)
}

# Componente principal (SEM usar <<- e sem estado global mutável)
#' @export
swiper <- function(
  id, ...,
  parent.style = "",
  width = "600px",
  height = "300px",
  loop = FALSE,
  direction = c("horizontal", "vertical"),
  speed = 400,
  spaceBetween = 100,
  mousewheel = FALSE,
  navigation = FALSE,
  pagination = FALSE
) {
  direction <- match.arg(direction)

  # container (id externo para “escopar” a instância)
  container <- htmltools$tags$div(
    class = "swiper-container",
    style = paste0("width:", width, ";height:", height, ";position:relative;"),
    # wrapper com os slides
    htmltools$tags$div(class = "swiper-wrapper", ...),
    # paginação
    if (isTRUE(pagination)) htmltools$tags$div(class = "swiper-pagination"),
    # botões
    if (isTRUE(navigation)) htmltools$tagList(
      htmltools$tags$div(class = "swiper-button-prev"),
      htmltools$tags$div(class = "swiper-button-next")
    ),
    # scrollbar (vamos ocultar)
    htmltools$tags$div(class = "swiper-scrollbar")
  )

  # Blocos opcionais de config (inseridos só quando TRUE)
  nav_js <- if (isTRUE(navigation))
    "navigation: { nextEl: '#%ID% .swiper-button-next', prevEl: '#%ID% .swiper-button-prev' }," else ""

  pag_js <- if (isTRUE(pagination))
    "pagination: { el: '#%ID% .swiper-pagination', dynamicBullets: true }," else ""

  # Script de inicialização — guarda a instância direto no elemento com id
  # Atualização: liberar cliques/foco dentro do Swiper para <select>, inputs etc.
  js <- sprintf("
    $(function(){
      setTimeout(function(){
        var scope = document.getElementById('%1$s');
        if(!scope) return;
        var el = scope.querySelector('.swiper-container');
        var inst = new Swiper(el, {
          effect: 'slide',
          grabCursor: false,
          centeredSlides: false,
          slidesPerView: 'auto',
          direction: '%2$s',
          loop: %3$s,
          speed: %4$d,
          spaceBetween: %5$d,
          mousewheel: %6$s,

          // 👇 Libera interação nativa com controles de formulário
          allowTouchMove: false,
          simulateTouch: false,
          preventClicks: false,
          preventClicksPropagation: false,
          touchStartPreventDefault: false,
          nested: true,

          %7$s
          %8$s
        });
        scope.swiper = inst; // <— guardamos aqui

        // Esconde a scrollbar original
        $('#%1$s .swiper-scrollbar').hide();

        // Blindagem: não deixe eventos 'descerem' para o Swiper nos controles
        $('#%1$s').on('touchstart mousedown pointerdown click',
          'select, input, textarea, button, label',
          function(e){ e.stopPropagation(); });

        // Reverte estilos que às vezes atrapalham interação
        $('#%1$s .swiper-slide, #%1$s .swiper-slide *').css({
          'user-select':'auto',
          '-webkit-user-select':'auto',
          'touch-action':'manipulation'
        });
      }, 300);
    });
  ",
    id,
    direction,
    tolower(as.character(loop)),
    as.integer(speed),
    as.integer(spaceBetween),
    tolower(as.character(mousewheel)),
    sub('%ID%', id, nav_js),
    sub('%ID%', id, pag_js)
  )

  tag <- htmltools$div(
    id = id,
    style = parent.style,
    htmltools$tags$script(js),
    container
  )

  htmltools$attachDependencies(tag, swiperDep())
}

# Controles — só chamam a instância JS guardada em '#id'.swiper
#' @export
swiperSlideNext <- function(id, speed = 1000) {
  shinyjs$runjs(sprintf(
    "var s = document.getElementById('%s'); if(s && s.swiper){ s.swiper.slideNext(%d); }",
    id, as.integer(speed)
  ))
  moveScrollToUp()
}

#' @export
swiperSlidePrevious <- function(id, speed = 1000) {
  shinyjs$runjs(sprintf(
    "var s = document.getElementById('%s'); if(s && s.swiper){ s.swiper.slidePrev(%d); }",
    id, as.integer(speed)
  ))
  moveScrollToUp()
}

#' @export
swiperSlideTo <- function(id, index, speed = 1000) {
  shinyjs$runjs(sprintf(
    "var s = document.getElementById('%s'); if(s && s.swiper){ s.swiper.slideTo(%d,%d); }",
    id, as.integer(index), as.integer(speed)
  ))
  moveScrollToUp()
}

#' @export
swiperDestroy <- function(id) {
  shinyjs$runjs(sprintf(
    "var s = document.getElementById('%s'); if(s && s.swiper){ s.swiper.destroy(true,true); s.swiper = null; }",
    id
  ))
}

