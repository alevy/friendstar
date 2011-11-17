$(function() {
  $("a[data-method]").click(function() {
    var method = $(this).attr("data-method");
    var form = $("<form/>");
    form.attr("action", $(this).attr("href"));
    form.attr("method", method);
    form.submit();
    return false;
  })
})