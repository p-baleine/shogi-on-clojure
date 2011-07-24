function highlight_move(event) {
    var piece$ = $(this);
    var square$ = $(this).parent();
    
    $.getJSON("/srv-highlight-moves",
              { from: square$.attr("id") },
              function(response) {
                  $.each(response, function() {
                          $("td.square#" + this.to)
                              .effect("highlight", { color: "blue" }, 1000);
                      });
              });
}

function opponent_player(player) {
    if (player == "first")
        return "second";
    else
        return "first";
}

function promote(piece) {
    $("img", piece)
        .attr("src",
              function(idx, val) {
                  return val.replace(/\.([^.]+)$/, "_promote." + "$1");
              });
}

function un_promote(piece) {
    $("img", piece)
        .attr("src",
              function(idx, val) {
                  return val.replace(/_promote(\..+)$/, "$1");
              });
}

function promoted_p(piece) {
    return $("img", piece).attr("src").match(/.+_promote.+/);
}

function make_move(player, from, to, promotion) {
    var from_square$ = $("#" + from);
    var to_square$ = $("#" + to);
    var from_piece$ = $("div.piece", from_square$);
    var to_piece$ = $("div.piece", to_square$);

    if (promotion) {
        promote(from_piece$);
    }

    $.post("/srv-make-move",
           { player: player, from: from, to: to, promotion: promotion },
           function(response) {
               if (eval(response).captured) {
                   var capture_square$ = $("table.captured." + player + " td")
                       .not(":has(img)")
                       .first();
                   var capture_piece$ = $("div.piece", capture_square$);
                   if (promoted_p(to_piece$)) {
                       un_promote(to_piece$);
                   }
                   capture_square$
                       .append(to_piece$
                               .removeClass(opponent_player(player))
                               .addClass(player));
                   from_square$.append(capture_piece$);
                   to_square$.append(from_piece$);
               } else {
                   from_square$.append(to_piece$);
                   to_square$.append(from_piece$);
               }
               from_piece$.css({left: 0, top: 0});
           },
           "json");
}

function get_move_srv(player) {
    // $.getJSON("/srv-random-strategy",
    $.getJSON("/srv-minimax-searcher",
              { player: player },
              function(response) {
                  make_move(player, response.from, response.to, response.promotion);
              });
}

function promotable_p(player, from, to, fn) {
    $.getJSON("/srv-promotable-p",
              { player: player, from: from, to: to },
              fn);
}

function valid_move_p(player, from, to, promotion, fn) {
    $.getJSON("/srv-valid-move-p",
              { player: player, from: from, to: to, promotion: promotion },
              fn);
}

function get_move_human(event, ui) {
    var from = $("td.square").has(ui.draggable).attr("id");
    var to = $(this).attr("id");
    var player = "second";

    promotable_p(player, from, to,
                 function(promotion) {
                     valid_move_p(player, from, to, promotion,
                                  function (response) {
                                      if (response) {
                                          make_move(player, from, to, promotion);
                                          get_move_srv("first");
                                      } else {
                                          alert("you can't put there.");
                                          ui.draggable.css({left: 0, top: 0});
                                      }
                                  });
                 });
}

$(function() {
        $("td.square").droppable({ drop: get_move_human });
        $("div.piece").draggable().mouseenter(highlight_move);
    });
