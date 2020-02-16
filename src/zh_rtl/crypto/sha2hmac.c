/*-
 * HMAC-SHA-224/256/384/512 implementation
 * Last update: 2005-06-15
 * Issue date:  2005-06-15
 *
 * Copyright (C) 2005 Olivier Gay <olivier.gay@a3.epfl.ch>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the project nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE PROJECT AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE PROJECT OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#include "zh_api.h"
#include "zh_crypto.h"

/* HMAC-SHA-224 functions */

void zh_hmac_sha224_init(zh_hmac_sha224_ctx *ctx, const void *keyv,
                         ZH_SIZE key_size)
{
    ZH_SIZE num;

    const unsigned char *key = ( const unsigned char * ) keyv;
    const unsigned char *key_used;
    unsigned char key_temp[ZH_SHA224_DIGEST_SIZE];
    ZH_SIZE i;

    if (key_size == ZH_SHA224_BLOCK_SIZE) {
        key_used = key;
        num = ZH_SHA224_BLOCK_SIZE;
    } else {
        ZH_SIZE fill;
        if (key_size > ZH_SHA224_BLOCK_SIZE){
            zh_sha224(key, key_size, key_temp);
            key_used = key_temp;
            num = ZH_SHA224_DIGEST_SIZE;
        } else { /* key_size > ZH_SHA224_BLOCK_SIZE */
            key_used = key;
            num = key_size;
        }
        fill = ZH_SHA224_BLOCK_SIZE - num;

        memset(ctx->block_ipad + num, 0x36, fill);
        memset(ctx->block_opad + num, 0x5c, fill);
    }

    for (i = 0; i < num; i++) {
        ctx->block_ipad[i] = key_used[i] ^ 0x36;
        ctx->block_opad[i] = key_used[i] ^ 0x5c;
    }

    zh_sha224_init(&ctx->ctx_inside);
    zh_sha224_update(&ctx->ctx_inside, ctx->block_ipad, ZH_SHA224_BLOCK_SIZE);

    zh_sha224_init(&ctx->ctx_outside);
    zh_sha224_update(&ctx->ctx_outside, ctx->block_opad,
                     ZH_SHA224_BLOCK_SIZE);

    /* for hmac_reinit */
    memcpy(&ctx->ctx_inside_reinit, &ctx->ctx_inside,
           sizeof(zh_sha224_ctx));
    memcpy(&ctx->ctx_outside_reinit, &ctx->ctx_outside,
           sizeof(zh_sha224_ctx));
}

void zh_hmac_sha224_reinit(zh_hmac_sha224_ctx *ctx)
{
    memcpy(&ctx->ctx_inside, &ctx->ctx_inside_reinit,
           sizeof(zh_sha224_ctx));
    memcpy(&ctx->ctx_outside, &ctx->ctx_outside_reinit,
           sizeof(zh_sha224_ctx));
}

void zh_hmac_sha224_update(zh_hmac_sha224_ctx *ctx, const void *message,
                           ZH_SIZE message_len)
{
    zh_sha224_update(&ctx->ctx_inside, message, message_len);
}

void zh_hmac_sha224_final(zh_hmac_sha224_ctx *ctx, unsigned char *mac,
                          ZH_SIZE mac_size)
{
    unsigned char digest_inside[ZH_SHA224_DIGEST_SIZE];
    unsigned char mac_temp[ZH_SHA224_DIGEST_SIZE];

    zh_sha224_final(&ctx->ctx_inside, digest_inside);
    zh_sha224_update(&ctx->ctx_outside, digest_inside, ZH_SHA224_DIGEST_SIZE);
    zh_sha224_final(&ctx->ctx_outside, mac_temp);
    memcpy(mac, mac_temp, mac_size);
}

void zh_hmac_sha224(const void *key, ZH_SIZE key_size,
          const void *message, ZH_SIZE message_len,
          unsigned char *mac, ZH_SIZE mac_size)
{
    zh_hmac_sha224_ctx ctx;

    zh_hmac_sha224_init(&ctx, key, key_size);
    zh_hmac_sha224_update(&ctx, message, message_len);
    zh_hmac_sha224_final(&ctx, mac, mac_size);
}

/* HMAC-SHA-256 functions */

void zh_hmac_sha256_init(zh_hmac_sha256_ctx *ctx, const void *keyv,
                         ZH_SIZE key_size)
{
    ZH_SIZE num;

    const unsigned char *key = ( const unsigned char * ) keyv;
    const unsigned char *key_used;
    unsigned char key_temp[ZH_SHA256_DIGEST_SIZE];
    ZH_SIZE i;

    if (key_size == ZH_SHA256_BLOCK_SIZE) {
        key_used = key;
        num = ZH_SHA256_BLOCK_SIZE;
    } else {
        ZH_SIZE fill;
        if (key_size > ZH_SHA256_BLOCK_SIZE){
            zh_sha256(key, key_size, key_temp);
            key_used = key_temp;
            num = ZH_SHA256_DIGEST_SIZE;
        } else { /* key_size > ZH_SHA256_BLOCK_SIZE */
            key_used = key;
            num = key_size;
        }
        fill = ZH_SHA256_BLOCK_SIZE - num;

        memset(ctx->block_ipad + num, 0x36, fill);
        memset(ctx->block_opad + num, 0x5c, fill);
    }

    for (i = 0; i < num; i++) {
        ctx->block_ipad[i] = key_used[i] ^ 0x36;
        ctx->block_opad[i] = key_used[i] ^ 0x5c;
    }

    zh_sha256_init(&ctx->ctx_inside);
    zh_sha256_update(&ctx->ctx_inside, ctx->block_ipad, ZH_SHA256_BLOCK_SIZE);

    zh_sha256_init(&ctx->ctx_outside);
    zh_sha256_update(&ctx->ctx_outside, ctx->block_opad,
                     ZH_SHA256_BLOCK_SIZE);

    /* for hmac_reinit */
    memcpy(&ctx->ctx_inside_reinit, &ctx->ctx_inside,
           sizeof(zh_sha256_ctx));
    memcpy(&ctx->ctx_outside_reinit, &ctx->ctx_outside,
           sizeof(zh_sha256_ctx));
}

void zh_hmac_sha256_reinit(zh_hmac_sha256_ctx *ctx)
{
    memcpy(&ctx->ctx_inside, &ctx->ctx_inside_reinit,
           sizeof(zh_sha256_ctx));
    memcpy(&ctx->ctx_outside, &ctx->ctx_outside_reinit,
           sizeof(zh_sha256_ctx));
}

void zh_hmac_sha256_update(zh_hmac_sha256_ctx *ctx, const void *message,
                           ZH_SIZE message_len)
{
    zh_sha256_update(&ctx->ctx_inside, message, message_len);
}

void zh_hmac_sha256_final(zh_hmac_sha256_ctx *ctx, unsigned char *mac,
                          ZH_SIZE mac_size)
{
    unsigned char digest_inside[ZH_SHA256_DIGEST_SIZE];
    unsigned char mac_temp[ZH_SHA256_DIGEST_SIZE];

    zh_sha256_final(&ctx->ctx_inside, digest_inside);
    zh_sha256_update(&ctx->ctx_outside, digest_inside, ZH_SHA256_DIGEST_SIZE);
    zh_sha256_final(&ctx->ctx_outside, mac_temp);
    memcpy(mac, mac_temp, mac_size);
}

void zh_hmac_sha256(const void *key, ZH_SIZE key_size,
          const void *message, ZH_SIZE message_len,
          unsigned char *mac, ZH_SIZE mac_size)
{
    zh_hmac_sha256_ctx ctx;

    zh_hmac_sha256_init(&ctx, key, key_size);
    zh_hmac_sha256_update(&ctx, message, message_len);
    zh_hmac_sha256_final(&ctx, mac, mac_size);
}

/* HMAC-SHA-384 functions */

void zh_hmac_sha384_init(zh_hmac_sha384_ctx *ctx, const void *keyv,
                         ZH_SIZE key_size)
{
    ZH_SIZE num;

    const unsigned char *key = ( const unsigned char * ) keyv;
    const unsigned char *key_used;
    unsigned char key_temp[ZH_SHA384_DIGEST_SIZE];
    ZH_SIZE i;

    if (key_size == ZH_SHA384_BLOCK_SIZE) {
        key_used = key;
        num = ZH_SHA384_BLOCK_SIZE;
    } else {
        ZH_SIZE fill;
        if (key_size > ZH_SHA384_BLOCK_SIZE){
            zh_sha384(key, key_size, key_temp);
            key_used = key_temp;
            num = ZH_SHA384_DIGEST_SIZE;
        } else { /* key_size > ZH_SHA384_BLOCK_SIZE */
            key_used = key;
            num = key_size;
        }
        fill = ZH_SHA384_BLOCK_SIZE - num;

        memset(ctx->block_ipad + num, 0x36, fill);
        memset(ctx->block_opad + num, 0x5c, fill);
    }

    for (i = 0; i < num; i++) {
        ctx->block_ipad[i] = key_used[i] ^ 0x36;
        ctx->block_opad[i] = key_used[i] ^ 0x5c;
    }

    zh_sha384_init(&ctx->ctx_inside);
    zh_sha384_update(&ctx->ctx_inside, ctx->block_ipad, ZH_SHA384_BLOCK_SIZE);

    zh_sha384_init(&ctx->ctx_outside);
    zh_sha384_update(&ctx->ctx_outside, ctx->block_opad,
                     ZH_SHA384_BLOCK_SIZE);

    /* for hmac_reinit */
    memcpy(&ctx->ctx_inside_reinit, &ctx->ctx_inside,
           sizeof(zh_sha384_ctx));
    memcpy(&ctx->ctx_outside_reinit, &ctx->ctx_outside,
           sizeof(zh_sha384_ctx));
}

void zh_hmac_sha384_reinit(zh_hmac_sha384_ctx *ctx)
{
    memcpy(&ctx->ctx_inside, &ctx->ctx_inside_reinit,
           sizeof(zh_sha384_ctx));
    memcpy(&ctx->ctx_outside, &ctx->ctx_outside_reinit,
           sizeof(zh_sha384_ctx));
}

void zh_hmac_sha384_update(zh_hmac_sha384_ctx *ctx, const void *message,
                           ZH_SIZE message_len)
{
    zh_sha384_update(&ctx->ctx_inside, message, message_len);
}

void zh_hmac_sha384_final(zh_hmac_sha384_ctx *ctx, unsigned char *mac,
                          ZH_SIZE mac_size)
{
    unsigned char digest_inside[ZH_SHA384_DIGEST_SIZE];
    unsigned char mac_temp[ZH_SHA384_DIGEST_SIZE];

    zh_sha384_final(&ctx->ctx_inside, digest_inside);
    zh_sha384_update(&ctx->ctx_outside, digest_inside, ZH_SHA384_DIGEST_SIZE);
    zh_sha384_final(&ctx->ctx_outside, mac_temp);
    memcpy(mac, mac_temp, mac_size);
}

void zh_hmac_sha384(const void *key, ZH_SIZE key_size,
          const void *message, ZH_SIZE message_len,
          unsigned char *mac, ZH_SIZE mac_size)
{
    zh_hmac_sha384_ctx ctx;

    zh_hmac_sha384_init(&ctx, key, key_size);
    zh_hmac_sha384_update(&ctx, message, message_len);
    zh_hmac_sha384_final(&ctx, mac, mac_size);
}

/* HMAC-SHA-512 functions */

void zh_hmac_sha512_init(zh_hmac_sha512_ctx *ctx, const void *keyv,
                         ZH_SIZE key_size)
{
    ZH_SIZE num;

    const unsigned char *key = ( const unsigned char * ) keyv;
    const unsigned char *key_used;
    unsigned char key_temp[ZH_SHA512_DIGEST_SIZE];
    ZH_SIZE i;

    if (key_size == ZH_SHA512_BLOCK_SIZE) {
        key_used = key;
        num = ZH_SHA512_BLOCK_SIZE;
    } else {
        ZH_SIZE fill;
        if (key_size > ZH_SHA512_BLOCK_SIZE){
            zh_sha512(key, key_size, key_temp);
            key_used = key_temp;
            num = ZH_SHA512_DIGEST_SIZE;
        } else { /* key_size > ZH_SHA512_BLOCK_SIZE */
            key_used = key;
            num = key_size;
        }
        fill = ZH_SHA512_BLOCK_SIZE - num;

        memset(ctx->block_ipad + num, 0x36, fill);
        memset(ctx->block_opad + num, 0x5c, fill);
    }

    for (i = 0; i < num; i++) {
        ctx->block_ipad[i] = key_used[i] ^ 0x36;
        ctx->block_opad[i] = key_used[i] ^ 0x5c;
    }

    zh_sha512_init(&ctx->ctx_inside);
    zh_sha512_update(&ctx->ctx_inside, ctx->block_ipad, ZH_SHA512_BLOCK_SIZE);

    zh_sha512_init(&ctx->ctx_outside);
    zh_sha512_update(&ctx->ctx_outside, ctx->block_opad,
                     ZH_SHA512_BLOCK_SIZE);

    /* for hmac_reinit */
    memcpy(&ctx->ctx_inside_reinit, &ctx->ctx_inside,
           sizeof(zh_sha512_ctx));
    memcpy(&ctx->ctx_outside_reinit, &ctx->ctx_outside,
           sizeof(zh_sha512_ctx));
}

void zh_hmac_sha512_reinit(zh_hmac_sha512_ctx *ctx)
{
    memcpy(&ctx->ctx_inside, &ctx->ctx_inside_reinit,
           sizeof(zh_sha512_ctx));
    memcpy(&ctx->ctx_outside, &ctx->ctx_outside_reinit,
           sizeof(zh_sha512_ctx));
}

void zh_hmac_sha512_update(zh_hmac_sha512_ctx *ctx, const void *message,
                           ZH_SIZE message_len)
{
    zh_sha512_update(&ctx->ctx_inside, message, message_len);
}

void zh_hmac_sha512_final(zh_hmac_sha512_ctx *ctx, unsigned char *mac,
                          ZH_SIZE mac_size)
{
    unsigned char digest_inside[ZH_SHA512_DIGEST_SIZE];
    unsigned char mac_temp[ZH_SHA512_DIGEST_SIZE];

    zh_sha512_final(&ctx->ctx_inside, digest_inside);
    zh_sha512_update(&ctx->ctx_outside, digest_inside, ZH_SHA512_DIGEST_SIZE);
    zh_sha512_final(&ctx->ctx_outside, mac_temp);
    memcpy(mac, mac_temp, mac_size);
}

void zh_hmac_sha512(const void *key, ZH_SIZE key_size,
          const void *message, ZH_SIZE message_len,
          unsigned char *mac, ZH_SIZE mac_size)
{
    zh_hmac_sha512_ctx ctx;

    zh_hmac_sha512_init(&ctx, key, key_size);
    zh_hmac_sha512_update(&ctx, message, message_len);
    zh_hmac_sha512_final(&ctx, mac, mac_size);
}
