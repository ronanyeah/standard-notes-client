const crypto = self.crypto.subtle;

const splitItemKey = str => ({
  itemEncryptionKey: str.substring(0, str.length / 2),
  itemAuthKey: str.substring(str.length / 2)
});

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

const getBits = async () => {
  const key = await crypto.importKey(
    "raw",
    self.crypto.getRandomValues(new Uint8Array(16)),
    { name: "PBKDF2" },
    false,
    ["deriveBits"]
  );

  const bits = await crypto.deriveBits(
    {
      name: "PBKDF2",
      salt: self.crypto.getRandomValues(new Uint8Array(16)),
      iterations: 100000,
      hash: { name: "SHA-512" }
    },
    key,
    512
  );

  return arrayBufferToHexString(bits);
};

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
    ["verify"]
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
    throw Error("not equal!");
  }

  const aes = await crypto.importKey(
    "raw",
    stringToBuffer1(encryptionKey),
    {
      name: "AES-CBC"
    },
    false,
    ["decrypt"]
  );

  const res = await crypto.decrypt(
    {
      name: "AES-CBC",
      iv: stringToBuffer1(iv)
    },
    aes,
    stringToBuffer2(atob(cipherText))
  );

  return arrayBufferToString(res);
};

const encryptString = async (data, uuid, authKey, encryptionKey) => {
  const iv = self.crypto.getRandomValues(new Uint8Array(16));
  const ivStr = arrayBufferToHexString(iv);

  const aesKey = await crypto.importKey(
    "raw",
    stringToBuffer1(encryptionKey),
    {
      name: "AES-CBC"
    },
    false,
    ["encrypt"]
  );

  const encryptedContent = await crypto.encrypt(
    {
      name: "AES-CBC",
      iv
    },
    aesKey,
    stringToBuffer2(data)
  );

  const contentCiphertext = btoa(arrayBufferToString(encryptedContent));
  const ciphertextToHash = ["002", uuid, ivStr, contentCiphertext].join(":");

  const hashKey = await crypto.importKey(
    "raw",
    stringToBuffer1(authKey),
    {
      name: "HMAC",
      hash: { name: "SHA-256" }
    },
    false,
    ["sign"]
  );

  const authHash = await crypto.sign(
    {
      name: "HMAC"
    },
    hashKey,
    stringToBuffer2(ciphertextToHash)
  );

  return [
    "002",
    arrayBufferToHexString(authHash),
    uuid,
    ivStr,
    contentCiphertext
  ].join(":");
};

const handlers = async request => {
  const url = request.url.substring((self.location.origin + "/crypto").length);

  switch (url) {
    case "/check": {
      return new Response(JSON.stringify("OK"));
    }
    case "/key": {
      const { password, salt, cost } = await request.json();
      const key = await stretchPassword(password, salt, cost);
      return new Response(JSON.stringify(key));
    }
    case "/decrypt-item": {
      const {
        content,
        encItemKey,
        authKey,
        encryptionKey
      } = await request.json();
      const { itemAuthKey, itemEncryptionKey } = splitItemKey(
        await decryptString(encItemKey, authKey, encryptionKey)
      );
      return new Response(
        await decryptString(content, itemAuthKey, itemEncryptionKey)
      );
    }
    case "/encrypt-item": {
      const { data, uuid, authKey, encryptionKey } = await request.json();
      const bits = await getBits();
      const { itemAuthKey, itemEncryptionKey } = splitItemKey(bits);
      const encryptedContent = await encryptString(
        JSON.stringify(data),
        uuid,
        itemAuthKey,
        itemEncryptionKey
      );
      const encItemKey = await encryptString(
        bits,
        uuid,
        authKey,
        encryptionKey
      );
      return new Response(JSON.stringify({ encryptedContent, encItemKey }));
    }
    default: {
      return new Response(JSON.stringify("bad request!"), { status: 400 });
    }
  }
};

self.addEventListener(
  "fetch",
  e =>
    e.request.url.startsWith(self.location.origin + "/crypto") &&
    e.request.method === "POST"
      ? e.respondWith(handlers(e.request))
      : e
);
