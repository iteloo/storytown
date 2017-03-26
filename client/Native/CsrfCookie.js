var _user$project$Native_CsrfCookie = function() {

  var scheduler = _elm_lang$core$Native_Scheduler;
  var value = {};

  value.csrfCookie = e => {
    return scheduler.nativeBinding(callback => {
      let r = document.cookie.match(new RegExp('XSRF-TOKEN=([^;]+)'));
      if (r) {
        callback(scheduler.succeed(r[1]));
      } else {
        callback(scheduler.fail([]));
      }
    })
  };

  return value;

}();
