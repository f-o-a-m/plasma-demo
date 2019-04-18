"use strict";

exports.nodeURLImpl = function() {
  return process.env.NODE_URL; // eslint-disable-line no-undef
};

exports.nodeNetworkIdImpl = function() {
  return process.env.NODE_NETWORK_ID; // eslint-disable-line no-undef
};
