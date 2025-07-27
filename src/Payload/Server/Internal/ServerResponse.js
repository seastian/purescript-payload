import { Readable } from "stream";

export const readableStreamToNodeReadable = (stream) => {
  return Readable.fromWeb(stream);
};

export const endResponse_ = (res) => (unit) => (cb) => () => {
  res.end(null, null, function () {
    cb(unit);
  });
};
