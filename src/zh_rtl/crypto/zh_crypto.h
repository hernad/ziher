/*
 * FIPS 180-2 SHA-224/256/384/512 implementation
 * Last update: 2007-02-02
 * Issue date:  2005-04-30
 * HMAC-SHA-224/256/384/512 implementation
 * Last update: 2005-06-15
 * Issue date:  2005-06-15
 *
 * Copyright (C) 2005, 2007 Olivier Gay <olivier.gay@a3.epfl.ch>
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

#ifndef ZH_CRYPTO_H_
#define ZH_CRYPTO_H_

#include "zh_api.h"

#define ZH_SHA224_DIGEST_SIZE  (  224 / 8 )
#define ZH_SHA256_DIGEST_SIZE  (  256 / 8 )
#define ZH_SHA384_DIGEST_SIZE  (  384 / 8 )
#define ZH_SHA512_DIGEST_SIZE  (  512 / 8 )

#define ZH_SHA256_BLOCK_SIZE   (  512 / 8 )
#define ZH_SHA512_BLOCK_SIZE   ( 1024 / 8 )
#define ZH_SHA384_BLOCK_SIZE   ZH_SHA512_BLOCK_SIZE
#define ZH_SHA224_BLOCK_SIZE   ZH_SHA256_BLOCK_SIZE

ZH_EXTERN_BEGIN

typedef struct {
   ZH_SIZE tot_len;
   ZH_SIZE len;
   unsigned char block[ 2 * ZH_SHA256_BLOCK_SIZE ];
   ZH_U32 h[ 8 ];
} zh_sha256_ctx;

typedef struct {
   ZH_SIZE tot_len;
   ZH_SIZE len;
   unsigned char block[ 2 * ZH_SHA512_BLOCK_SIZE ];
   ZH_U64 h[ 8 ];
} zh_sha512_ctx;

typedef zh_sha512_ctx zh_sha384_ctx;
typedef zh_sha256_ctx zh_sha224_ctx;

extern ZH_EXPORT void zh_sha224_init( zh_sha224_ctx * ctx );
extern ZH_EXPORT void zh_sha224_update( zh_sha224_ctx * ctx,
                                        const void * message,
                                        ZH_SIZE len );
extern ZH_EXPORT void zh_sha224_final( zh_sha224_ctx * ctx,
                                       unsigned char * digest );
extern ZH_EXPORT void zh_sha224( const void * message,
                                 ZH_SIZE len,
                                 unsigned char * digest );

extern ZH_EXPORT void zh_sha256_init( zh_sha256_ctx * ctx );
extern ZH_EXPORT void zh_sha256_update( zh_sha256_ctx * ctx,
                                       const void *message,
                                       ZH_SIZE len );
extern ZH_EXPORT void zh_sha256_final( zh_sha256_ctx * ctx,
                                       unsigned char * digest );
extern ZH_EXPORT void zh_sha256( const void * message,
                                 ZH_SIZE len,
                                 unsigned char * digest );

extern ZH_EXPORT void zh_sha384_init( zh_sha384_ctx * ctx );
extern ZH_EXPORT void zh_sha384_update( zh_sha384_ctx * ctx,
                                        const void *message,
                                        ZH_SIZE len );
extern ZH_EXPORT void zh_sha384_final( zh_sha384_ctx * ctx,
                                       unsigned char * digest );
extern ZH_EXPORT void zh_sha384( const void * message,
                                 ZH_SIZE len,
                                 unsigned char * digest );

extern ZH_EXPORT void zh_sha512_init( zh_sha512_ctx * ctx );
extern ZH_EXPORT void zh_sha512_update( zh_sha512_ctx * ctx,
                                        const void *message,
                                        ZH_SIZE len );
extern ZH_EXPORT void zh_sha512_final( zh_sha512_ctx * ctx,
                                       unsigned char * digest );
extern ZH_EXPORT void zh_sha512( const void * message,
                                 ZH_SIZE len,
                                 unsigned char * digest );

typedef struct {
   zh_sha224_ctx ctx_inside;
   zh_sha224_ctx ctx_outside;

   /* for hmac_reinit */
   zh_sha224_ctx ctx_inside_reinit;
   zh_sha224_ctx ctx_outside_reinit;

   unsigned char block_ipad[ ZH_SHA224_BLOCK_SIZE ];
   unsigned char block_opad[ ZH_SHA224_BLOCK_SIZE ];
} zh_hmac_sha224_ctx;

typedef struct {
   zh_sha256_ctx ctx_inside;
   zh_sha256_ctx ctx_outside;

   /* for hmac_reinit */
   zh_sha256_ctx ctx_inside_reinit;
   zh_sha256_ctx ctx_outside_reinit;

   unsigned char block_ipad[ ZH_SHA256_BLOCK_SIZE ];
   unsigned char block_opad[ ZH_SHA256_BLOCK_SIZE ];
} zh_hmac_sha256_ctx;

typedef struct {
   zh_sha384_ctx ctx_inside;
   zh_sha384_ctx ctx_outside;

   /* for hmac_reinit */
   zh_sha384_ctx ctx_inside_reinit;
   zh_sha384_ctx ctx_outside_reinit;

   unsigned char block_ipad[ ZH_SHA384_BLOCK_SIZE ];
   unsigned char block_opad[ ZH_SHA384_BLOCK_SIZE ];
} zh_hmac_sha384_ctx;

typedef struct {
   zh_sha512_ctx ctx_inside;
   zh_sha512_ctx ctx_outside;

   /* for hmac_reinit */
   zh_sha512_ctx ctx_inside_reinit;
   zh_sha512_ctx ctx_outside_reinit;

   unsigned char block_ipad[ ZH_SHA512_BLOCK_SIZE ];
   unsigned char block_opad[ ZH_SHA512_BLOCK_SIZE ];
} zh_hmac_sha512_ctx;

extern ZH_EXPORT void zh_hmac_sha224_init( zh_hmac_sha224_ctx * ctx,
                                           const void * key,
                                           ZH_SIZE key_size );
extern ZH_EXPORT void zh_hmac_sha224_reinit( zh_hmac_sha224_ctx * ctx );
extern ZH_EXPORT void zh_hmac_sha224_update( zh_hmac_sha224_ctx * ctx,
                                             const void * message,
                                             ZH_SIZE message_len );
extern ZH_EXPORT void zh_hmac_sha224_final( zh_hmac_sha224_ctx * ctx,
                                            unsigned char * mac,
                                            ZH_SIZE mac_size );
extern ZH_EXPORT void zh_hmac_sha224( const void * key,
                                      ZH_SIZE key_size,
                                      const void * message,
                                      ZH_SIZE message_len,
                                      unsigned char * mac,
                                      ZH_SIZE mac_size );

extern ZH_EXPORT void zh_hmac_sha256_init( zh_hmac_sha256_ctx * ctx,
                                           const void * key,
                                           ZH_SIZE key_size );
extern ZH_EXPORT void zh_hmac_sha256_reinit( zh_hmac_sha256_ctx * ctx );
extern ZH_EXPORT void zh_hmac_sha256_update( zh_hmac_sha256_ctx * ctx,
                                             const void * message,
                                             ZH_SIZE message_len );
extern ZH_EXPORT void zh_hmac_sha256_final( zh_hmac_sha256_ctx * ctx,
                                            unsigned char * mac,
                                            ZH_SIZE mac_size );
extern ZH_EXPORT void zh_hmac_sha256( const void * key,
                                      ZH_SIZE key_size,
                                      const void * message,
                                      ZH_SIZE message_len,
                                      unsigned char * mac,
                                      ZH_SIZE mac_size );

extern ZH_EXPORT void zh_hmac_sha384_init( zh_hmac_sha384_ctx * ctx,
                                           const void * key,
                                           ZH_SIZE key_size );
extern ZH_EXPORT void zh_hmac_sha384_reinit( zh_hmac_sha384_ctx * ctx );
extern ZH_EXPORT void zh_hmac_sha384_update( zh_hmac_sha384_ctx * ctx,
                                             const void * message,
                                             ZH_SIZE message_len );
extern ZH_EXPORT void zh_hmac_sha384_final( zh_hmac_sha384_ctx * ctx,
                                            unsigned char * mac,
                                            ZH_SIZE mac_size );
extern ZH_EXPORT void zh_hmac_sha384( const void * key,
                                      ZH_SIZE key_size,
                                      const void * message,
                                      ZH_SIZE message_len,
                                      unsigned char * mac,
                                      ZH_SIZE mac_size );

extern ZH_EXPORT void zh_hmac_sha512_init( zh_hmac_sha512_ctx * ctx,
                                           const void * key,
                                           ZH_SIZE key_size );
extern ZH_EXPORT void zh_hmac_sha512_reinit( zh_hmac_sha512_ctx * ctx );
extern ZH_EXPORT void zh_hmac_sha512_update( zh_hmac_sha512_ctx * ctx,
                                             const void * message,
                                             ZH_SIZE message_len );
extern ZH_EXPORT void zh_hmac_sha512_final( zh_hmac_sha512_ctx * ctx,
                                            unsigned char * mac,
                                            ZH_SIZE mac_size );
extern ZH_EXPORT void zh_hmac_sha512( const void * key,
                                      ZH_SIZE key_size,
                                      const void * message,
                                      ZH_SIZE message_len,
                                      unsigned char * mac,
                                      ZH_SIZE mac_size );

ZH_EXTERN_END

#endif /* ZH_CRYPTO_H_ */
