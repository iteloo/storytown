// var _user$project$Native_FileReader = function() {
var _user$project$Native_MediaRecorder = function() {

  let media = {
        type: 'audio/ogg',
        gUM: {audio: true}
      },
      stream,
      recorder,
      cb,
      chunks;

  navigator.mediaDevices.getUserMedia(media.gUM).then(_stream => {
    stream = _stream;
    recorder = new MediaRecorder(stream);
    recorder.ondataavailable = e => {
      chunks.push(e.data);
      if(recorder.state == 'inactive') {
        let blob = new Blob(chunks, {type: media.type })
          , url = URL.createObjectURL(blob)
          ;
          cb(scheduler.succeed({ ctor: '(,)', _0: url, _1: blob }));
      };
    };
    console.log('got media successfully');
  }).catch(console.log);

  var scheduler = _elm_lang$core$Native_Scheduler;

  var start = function(e){
      return scheduler.nativeBinding(function(callback) {
        chunks=[];
        recorder.start();
        console.log("starting recorder")
        callback(scheduler.succeed([]))
      });
  };

  var stop = function(e){
      return scheduler.nativeBinding(function(callback) {
        cb = callback;
        recorder.stop();
        console.log("stopping recorder")
      });
  };

  var blobPart = function(name, blob) {
    return {
        _0: name,
        _1: blob
    }
  };

  var blobBody = function(blob) {
    return {
      ctor : 'StringBody',
      _0: blob.type,
      _1: blob
    }
  }

  return {
      start : start,
      stop : stop,
      blobPart : F2(blobPart),
      blobBody : blobBody
  };
}();
