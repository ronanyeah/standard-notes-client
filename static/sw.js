const crypto = self.crypto.subtle;

const stringToBuffer1 = string =>
  // https://stackoverflow.com/questions/43131242/how-to-convert-a-hexademical-string-of-data-to-an-arraybuffer-in-javascript
  new Uint8Array(string.match(/[\da-f]{2}/gi).map(h => parseInt(h, 16)));

const stringToBuffer2 = string =>
  new Uint8Array(string.split("").map(x => x.charCodeAt(0)));

const arrayBufferToHexString = buffer =>
  new Uint8Array(buffer).reduce((acc, v) => {
    const byte = v.toString(16);

    return byte.length < 2 ? acc + "0" + byte : acc + byte;
  }, "");

const arrayBufferToString = buffer =>
  new Uint8Array(buffer).reduce(
    (acc, val) => acc + String.fromCharCode(val),
    ""
  );

const stretchPassword = async (password, salt, cost) => {
  const key = await crypto.importKey(
    "raw",
    stringToBuffer2(password),
    { name: "PBKDF2" },
    false,
    ["deriveBits"]
  );

  const bits = await crypto.deriveBits(
    {
      name: "PBKDF2",
      salt: stringToBuffer2(salt),
      iterations: cost,
      hash: { name: "SHA-512" }
    },
    key,
    768
  );

  return arrayBufferToHexString(bits);
};

const decryptString = async (str, authKey, encryptionKey) => {
  const [version, authHash, uuid, iv, cipherText] = str.split(":");

  const strToHash = [version, uuid, iv, cipherText].join(":");

  const key = await crypto.importKey(
    "raw",
    stringToBuffer1(authKey),
    {
      name: "HMAC",
      hash: { name: "SHA-256" }
    },
    false,
    ["sign", "verify"]
  );

  const eq = await crypto.verify(
    {
      name: "HMAC"
    },
    key,
    stringToBuffer1(authHash),
    stringToBuffer2(strToHash)
  );

  if (!eq) {
    return new Response(JSON.stringify("not equal!"), { status: 400 });
  }

  const aes = await crypto.importKey(
    "raw",
    stringToBuffer1(encryptionKey),
    {
      name: "AES-CBC"
    },
    false,
    ["encrypt", "decrypt"]
  );

  const res = await crypto.decrypt(
    {
      name: "AES-CBC",
      iv: stringToBuffer1(iv)
    },
    aes,
    stringToBuffer2(atob(cipherText))
  );

  return new Response(JSON.stringify(arrayBufferToString(res)));
};

const handlers = async request => {
  const url = request.url.substring((self.location.origin + "/crypto").length);

  switch (url) {
    case "/key": {
      const { password, salt, cost } = await request.json();
      const key = await stretchPassword(password, salt, cost);
      return new Response(JSON.stringify(key));
    }
    case "/decrypt": {
      const { text, authKey, encryptionKey } = await request.json();
      return decryptString(text, authKey, encryptionKey);
    }
    default: {
      return fetch(request);
    }
  }
};

self.addEventListener("fetch", e =>
  e.respondWith(
    e.request.url.startsWith(self.location.origin + "/crypto") &&
    e.request.method === "POST"
      ? handlers(e.request)
      : fetch(e.request)
  )
);
