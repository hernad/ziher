/*
 * OpenSSL API (EVP CIPHER) - Ziher interface.
 *
 * Copyright 2009-2016 Viktor Szakats (vszakats.net/ziher)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
 *
 * As a special exception, the Ziher Project gives permission for
 * additional uses of the text contained in its release of Ziher.
 *
 * The exception is that, if you link the Ziher libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Ziher library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Ziher
 * Project under the name Ziher.  If you copy code from other
 * Ziher Project or Free Software Foundation releases into a copy of
 * Ziher, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Ziher, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include "zh_ssl.h"

#include "zh_item_api.h"

#include <openssl/evp.h>

ZH_FUNC( OPENSSL_ADD_ALL_CIPHERS )
{
   OpenSSL_add_all_ciphers();
}

static ZH_GARBAGE_FUNC( EVP_CIPHER_CTX_release )
{
   void ** ph = ( void ** ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( ph && *ph )
   {
#if OPENSSL_VERSION_NUMBER >= 0x10100000L
      EVP_CIPHER_CTX_free( ( EVP_CIPHER_CTX * ) *ph );
#else
      /* Cleanup the object */
      EVP_CIPHER_CTX_cleanup( ( EVP_CIPHER_CTX * ) *ph );
      /* Destroy the object */
      zh_xfree( *ph );
#endif

      /* set pointer to NULL just in case */
      *ph = NULL;
   }
}

static const ZH_GC_FUNCS s_gcEVP_CIPHER_CTX_funcs =
{
   EVP_CIPHER_CTX_release,
   zh_gcDummyMark
};

static ZH_BOOL zh_EVP_CIPHER_CTX_is( int iParam )
{
   return zh_parptrGC( &s_gcEVP_CIPHER_CTX_funcs, iParam ) != NULL;
}

static EVP_CIPHER_CTX * zh_EVP_CIPHER_CTX_par( int iParam )
{
   void ** ph = ( void ** ) zh_parptrGC( &s_gcEVP_CIPHER_CTX_funcs, iParam );

   return ph ? ( EVP_CIPHER_CTX * ) *ph : NULL;
}

ZH_BOOL zh_EVP_CIPHER_is( int iParam )
{
   return ZH_ISCHAR( iParam ) || ZH_IS_PARAM_NUM( iParam );
}

const EVP_CIPHER * zh_EVP_CIPHER_par( int iParam )
{
   const EVP_CIPHER * p;

   if( ZH_ISCHAR( iParam ) )
      return EVP_get_cipherbyname( zh_parc( iParam ) );

   switch( zh_parni( iParam ) )
   {
      case ZH_EVP_CIPHER_ENC_NULL:             p = EVP_enc_null();            break;
#ifndef OPENSSL_NO_DES
      case ZH_EVP_CIPHER_DES_ECB:              p = EVP_des_ecb();             break;
      case ZH_EVP_CIPHER_DES_EDE:              p = EVP_des_ede();             break;
      case ZH_EVP_CIPHER_DES_EDE3:             p = EVP_des_ede3();            break;
#if OPENSSL_VERSION_NUMBER >= 0x00907000L
      case ZH_EVP_CIPHER_DES_EDE_ECB:          p = EVP_des_ede_ecb();         break;
      case ZH_EVP_CIPHER_DES_EDE3_ECB:         p = EVP_des_ede3_ecb();        break;
#endif
      case ZH_EVP_CIPHER_DES_CFB:              p = EVP_des_cfb();             break;
      case ZH_EVP_CIPHER_DES_EDE_CFB:          p = EVP_des_ede_cfb();         break;
      case ZH_EVP_CIPHER_DES_EDE3_CFB:         p = EVP_des_ede3_cfb();        break;
#if OPENSSL_VERSION_NUMBER >= 0x00907050L
      case ZH_EVP_CIPHER_DES_CFB1:             p = EVP_des_cfb1();            break;
      case ZH_EVP_CIPHER_DES_CFB8:             p = EVP_des_cfb8();            break;
      case ZH_EVP_CIPHER_DES_CFB64:            p = EVP_des_cfb64();           break;
      case ZH_EVP_CIPHER_DES_EDE_CFB64:        p = EVP_des_ede_cfb64();       break;
      case ZH_EVP_CIPHER_DES_EDE3_CFB1:        p = EVP_des_ede3_cfb1();       break;
      case ZH_EVP_CIPHER_DES_EDE3_CFB8:        p = EVP_des_ede3_cfb8();       break;
      case ZH_EVP_CIPHER_DES_EDE3_CFB64:       p = EVP_des_ede3_cfb64();      break;
#endif
      case ZH_EVP_CIPHER_DES_OFB:              p = EVP_des_ofb();             break;
      case ZH_EVP_CIPHER_DES_EDE_OFB:          p = EVP_des_ede_ofb();         break;
      case ZH_EVP_CIPHER_DES_EDE3_OFB:         p = EVP_des_ede3_ofb();        break;
      case ZH_EVP_CIPHER_DES_CBC:              p = EVP_des_cbc();             break;
      case ZH_EVP_CIPHER_DES_EDE_CBC:          p = EVP_des_ede_cbc();         break;
      case ZH_EVP_CIPHER_DES_EDE3_CBC:         p = EVP_des_ede3_cbc();        break;
      case ZH_EVP_CIPHER_DESX_CBC:             p = EVP_desx_cbc();            break;
#endif
#ifndef OPENSSL_NO_RC4
      case ZH_EVP_CIPHER_RC4:                  p = EVP_rc4();                 break;
      case ZH_EVP_CIPHER_RC4_40:               p = EVP_rc4_40();              break;
#endif
#ifndef OPENSSL_NO_RC2
      case ZH_EVP_CIPHER_RC2_ECB:              p = EVP_rc2_ecb();             break;
      case ZH_EVP_CIPHER_RC2_CBC:              p = EVP_rc2_cbc();             break;
      case ZH_EVP_CIPHER_RC2_40_CBC:           p = EVP_rc2_40_cbc();          break;
      case ZH_EVP_CIPHER_RC2_64_CBC:           p = EVP_rc2_64_cbc();          break;
#if OPENSSL_VERSION_NUMBER >= 0x00907050L
      case ZH_EVP_CIPHER_RC2_CFB64:            p = EVP_rc2_cfb64();           break;
#endif
      case ZH_EVP_CIPHER_RC2_CFB:              p = EVP_rc2_cfb();             break;
      case ZH_EVP_CIPHER_RC2_OFB:              p = EVP_rc2_ofb();             break;
#endif
#ifndef OPENSSL_NO_BF
      case ZH_EVP_CIPHER_BF_ECB:               p = EVP_bf_ecb();              break;
      case ZH_EVP_CIPHER_BF_CBC:               p = EVP_bf_cbc();              break;
#if OPENSSL_VERSION_NUMBER >= 0x00907050L
      case ZH_EVP_CIPHER_BF_CFB64:             p = EVP_bf_cfb64();            break;
#endif
      case ZH_EVP_CIPHER_BF_CFB:               p = EVP_bf_cfb();              break;
      case ZH_EVP_CIPHER_BF_OFB:               p = EVP_bf_ofb();              break;
#endif
#ifndef OPENSSL_NO_AES
#if OPENSSL_VERSION_NUMBER >= 0x10001000L
      case ZH_EVP_CIPHER_AES_128_GCM:          p = EVP_aes_128_gcm();         break;
#endif
      case ZH_EVP_CIPHER_AES_128_ECB:          p = EVP_aes_128_ecb();         break;
      case ZH_EVP_CIPHER_AES_128_CBC:          p = EVP_aes_128_cbc();         break;
#if OPENSSL_VERSION_NUMBER >= 0x00907050L
      case ZH_EVP_CIPHER_AES_128_CFB1:         p = EVP_aes_128_cfb1();        break;
      case ZH_EVP_CIPHER_AES_128_CFB8:         p = EVP_aes_128_cfb8();        break;
      case ZH_EVP_CIPHER_AES_128_CFB128:       p = EVP_aes_128_cfb128();      break;
#endif
      case ZH_EVP_CIPHER_AES_128_CFB:          p = EVP_aes_128_cfb();         break;
      case ZH_EVP_CIPHER_AES_128_OFB:          p = EVP_aes_128_ofb();         break;
#if OPENSSL_VERSION_NUMBER >= 0x10001000L
      case ZH_EVP_CIPHER_AES_192_GCM:          p = EVP_aes_192_gcm();         break;
#endif
      case ZH_EVP_CIPHER_AES_192_ECB:          p = EVP_aes_192_ecb();         break;
      case ZH_EVP_CIPHER_AES_192_CBC:          p = EVP_aes_192_cbc();         break;
#if OPENSSL_VERSION_NUMBER >= 0x00907050L
      case ZH_EVP_CIPHER_AES_192_CFB1:         p = EVP_aes_192_cfb1();        break;
      case ZH_EVP_CIPHER_AES_192_CFB8:         p = EVP_aes_192_cfb8();        break;
      case ZH_EVP_CIPHER_AES_192_CFB128:       p = EVP_aes_192_cfb128();      break;
#endif
      case ZH_EVP_CIPHER_AES_192_CFB:          p = EVP_aes_192_cfb();         break;
      case ZH_EVP_CIPHER_AES_192_OFB:          p = EVP_aes_192_ofb();         break;
#if OPENSSL_VERSION_NUMBER >= 0x10001000L
      case ZH_EVP_CIPHER_AES_256_GCM:          p = EVP_aes_256_gcm();         break;
#endif
      case ZH_EVP_CIPHER_AES_256_ECB:          p = EVP_aes_256_ecb();         break;
      case ZH_EVP_CIPHER_AES_256_CBC:          p = EVP_aes_256_cbc();         break;
#if OPENSSL_VERSION_NUMBER >= 0x00907050L
      case ZH_EVP_CIPHER_AES_256_CFB1:         p = EVP_aes_256_cfb1();        break;
      case ZH_EVP_CIPHER_AES_256_CFB8:         p = EVP_aes_256_cfb8();        break;
      case ZH_EVP_CIPHER_AES_256_CFB128:       p = EVP_aes_256_cfb128();      break;
#endif
      case ZH_EVP_CIPHER_AES_256_CFB:          p = EVP_aes_256_cfb();         break;
      case ZH_EVP_CIPHER_AES_256_OFB:          p = EVP_aes_256_ofb();         break;
#endif
#ifndef OPENSSL_NO_CAMELLIA
      case ZH_EVP_CIPHER_CAMELLIA_128_ECB:     p = EVP_camellia_128_ecb();    break;
      case ZH_EVP_CIPHER_CAMELLIA_128_CBC:     p = EVP_camellia_128_cbc();    break;
      case ZH_EVP_CIPHER_CAMELLIA_128_CFB1:    p = EVP_camellia_128_cfb1();   break;
      case ZH_EVP_CIPHER_CAMELLIA_128_CFB8:    p = EVP_camellia_128_cfb8();   break;
      case ZH_EVP_CIPHER_CAMELLIA_128_CFB128:  p = EVP_camellia_128_cfb128(); break;
      case ZH_EVP_CIPHER_CAMELLIA_128_CFB:     p = EVP_camellia_128_cfb();    break;
      case ZH_EVP_CIPHER_CAMELLIA_128_OFB:     p = EVP_camellia_128_ofb();    break;
      case ZH_EVP_CIPHER_CAMELLIA_192_ECB:     p = EVP_camellia_192_ecb();    break;
      case ZH_EVP_CIPHER_CAMELLIA_192_CBC:     p = EVP_camellia_192_cbc();    break;
      case ZH_EVP_CIPHER_CAMELLIA_192_CFB1:    p = EVP_camellia_192_cfb1();   break;
      case ZH_EVP_CIPHER_CAMELLIA_192_CFB8:    p = EVP_camellia_192_cfb8();   break;
      case ZH_EVP_CIPHER_CAMELLIA_192_CFB128:  p = EVP_camellia_192_cfb128(); break;
      case ZH_EVP_CIPHER_CAMELLIA_192_CFB:     p = EVP_camellia_192_cfb();    break;
      case ZH_EVP_CIPHER_CAMELLIA_192_OFB:     p = EVP_camellia_192_ofb();    break;
      case ZH_EVP_CIPHER_CAMELLIA_256_ECB:     p = EVP_camellia_256_ecb();    break;
      case ZH_EVP_CIPHER_CAMELLIA_256_CBC:     p = EVP_camellia_256_cbc();    break;
      case ZH_EVP_CIPHER_CAMELLIA_256_CFB1:    p = EVP_camellia_256_cfb1();   break;
      case ZH_EVP_CIPHER_CAMELLIA_256_CFB8:    p = EVP_camellia_256_cfb8();   break;
      case ZH_EVP_CIPHER_CAMELLIA_256_CFB128:  p = EVP_camellia_256_cfb128(); break;
      case ZH_EVP_CIPHER_CAMELLIA_256_CFB:     p = EVP_camellia_256_cfb();    break;
      case ZH_EVP_CIPHER_CAMELLIA_256_OFB:     p = EVP_camellia_256_ofb();    break;
#endif
      default:                                 p = NULL;
   }

   return p;
}

static int zh_EVP_CIPHER_ptr_to_id( const EVP_CIPHER * p )
{
   int n;

   if(      p == EVP_enc_null()            ) n = ZH_EVP_CIPHER_ENC_NULL;
#ifndef OPENSSL_NO_DES
   else if( p == EVP_des_ecb()             ) n = ZH_EVP_CIPHER_DES_ECB;
   else if( p == EVP_des_ede()             ) n = ZH_EVP_CIPHER_DES_EDE;
   else if( p == EVP_des_ede3()            ) n = ZH_EVP_CIPHER_DES_EDE3;
#if OPENSSL_VERSION_NUMBER >= 0x00907000L
   else if( p == EVP_des_ede_ecb()         ) n = ZH_EVP_CIPHER_DES_EDE_ECB;
   else if( p == EVP_des_ede3_ecb()        ) n = ZH_EVP_CIPHER_DES_EDE3_ECB;
#endif
   else if( p == EVP_des_cfb()             ) n = ZH_EVP_CIPHER_DES_CFB;
   else if( p == EVP_des_ede_cfb()         ) n = ZH_EVP_CIPHER_DES_EDE_CFB;
   else if( p == EVP_des_ede3_cfb()        ) n = ZH_EVP_CIPHER_DES_EDE3_CFB;
#if OPENSSL_VERSION_NUMBER >= 0x00907050L
   else if( p == EVP_des_cfb64()           ) n = ZH_EVP_CIPHER_DES_CFB64;
   else if( p == EVP_des_cfb1()            ) n = ZH_EVP_CIPHER_DES_CFB1;
   else if( p == EVP_des_cfb8()            ) n = ZH_EVP_CIPHER_DES_CFB8;
   else if( p == EVP_des_ede_cfb64()       ) n = ZH_EVP_CIPHER_DES_EDE_CFB64;
   else if( p == EVP_des_ede3_cfb64()      ) n = ZH_EVP_CIPHER_DES_EDE3_CFB64;
   else if( p == EVP_des_ede3_cfb1()       ) n = ZH_EVP_CIPHER_DES_EDE3_CFB1;
   else if( p == EVP_des_ede3_cfb8()       ) n = ZH_EVP_CIPHER_DES_EDE3_CFB8;
#endif
   else if( p == EVP_des_ofb()             ) n = ZH_EVP_CIPHER_DES_OFB;
   else if( p == EVP_des_ede_ofb()         ) n = ZH_EVP_CIPHER_DES_EDE_OFB;
   else if( p == EVP_des_ede3_ofb()        ) n = ZH_EVP_CIPHER_DES_EDE3_OFB;
   else if( p == EVP_des_cbc()             ) n = ZH_EVP_CIPHER_DES_CBC;
   else if( p == EVP_des_ede_cbc()         ) n = ZH_EVP_CIPHER_DES_EDE_CBC;
   else if( p == EVP_des_ede3_cbc()        ) n = ZH_EVP_CIPHER_DES_EDE3_CBC;
   else if( p == EVP_desx_cbc()            ) n = ZH_EVP_CIPHER_DESX_CBC;
#endif
#ifndef OPENSSL_NO_RC4
   else if( p == EVP_rc4()                 ) n = ZH_EVP_CIPHER_RC4;
   else if( p == EVP_rc4_40()              ) n = ZH_EVP_CIPHER_RC4_40;
#endif
#ifndef OPENSSL_NO_RC2
   else if( p == EVP_rc2_ecb()             ) n = ZH_EVP_CIPHER_RC2_ECB;
   else if( p == EVP_rc2_cbc()             ) n = ZH_EVP_CIPHER_RC2_CBC;
   else if( p == EVP_rc2_40_cbc()          ) n = ZH_EVP_CIPHER_RC2_40_CBC;
   else if( p == EVP_rc2_64_cbc()          ) n = ZH_EVP_CIPHER_RC2_64_CBC;
#if OPENSSL_VERSION_NUMBER >= 0x00907050L
   else if( p == EVP_rc2_cfb64()           ) n = ZH_EVP_CIPHER_RC2_CFB64;
#endif
   else if( p == EVP_rc2_cfb()             ) n = ZH_EVP_CIPHER_RC2_CFB;
   else if( p == EVP_rc2_ofb()             ) n = ZH_EVP_CIPHER_RC2_OFB;
#endif
#ifndef OPENSSL_NO_BF
   else if( p == EVP_bf_ecb()              ) n = ZH_EVP_CIPHER_BF_ECB;
   else if( p == EVP_bf_cbc()              ) n = ZH_EVP_CIPHER_BF_CBC;
#if OPENSSL_VERSION_NUMBER >= 0x00907050L
   else if( p == EVP_bf_cfb64()            ) n = ZH_EVP_CIPHER_BF_CFB64;
#endif
   else if( p == EVP_bf_cfb()              ) n = ZH_EVP_CIPHER_BF_CFB;
   else if( p == EVP_bf_ofb()              ) n = ZH_EVP_CIPHER_BF_OFB;
#endif
#ifndef OPENSSL_NO_AES
   else if( p == EVP_aes_128_ecb()         ) n = ZH_EVP_CIPHER_AES_128_ECB;
   else if( p == EVP_aes_128_cbc()         ) n = ZH_EVP_CIPHER_AES_128_CBC;
#if OPENSSL_VERSION_NUMBER >= 0x00907050L
   else if( p == EVP_aes_128_cfb1()        ) n = ZH_EVP_CIPHER_AES_128_CFB1;
   else if( p == EVP_aes_128_cfb8()        ) n = ZH_EVP_CIPHER_AES_128_CFB8;
   else if( p == EVP_aes_128_cfb128()      ) n = ZH_EVP_CIPHER_AES_128_CFB128;
#endif
   else if( p == EVP_aes_128_cfb()         ) n = ZH_EVP_CIPHER_AES_128_CFB;
   else if( p == EVP_aes_128_ofb()         ) n = ZH_EVP_CIPHER_AES_128_OFB;
   else if( p == EVP_aes_192_ecb()         ) n = ZH_EVP_CIPHER_AES_192_ECB;
   else if( p == EVP_aes_192_cbc()         ) n = ZH_EVP_CIPHER_AES_192_CBC;
#if OPENSSL_VERSION_NUMBER >= 0x00907050L
   else if( p == EVP_aes_192_cfb1()        ) n = ZH_EVP_CIPHER_AES_192_CFB1;
   else if( p == EVP_aes_192_cfb8()        ) n = ZH_EVP_CIPHER_AES_192_CFB8;
   else if( p == EVP_aes_192_cfb128()      ) n = ZH_EVP_CIPHER_AES_192_CFB128;
#endif
   else if( p == EVP_aes_192_cfb()         ) n = ZH_EVP_CIPHER_AES_192_CFB;
   else if( p == EVP_aes_192_ofb()         ) n = ZH_EVP_CIPHER_AES_192_OFB;
   else if( p == EVP_aes_256_ecb()         ) n = ZH_EVP_CIPHER_AES_256_ECB;
   else if( p == EVP_aes_256_cbc()         ) n = ZH_EVP_CIPHER_AES_256_CBC;
#if OPENSSL_VERSION_NUMBER >= 0x00907050L
   else if( p == EVP_aes_256_cfb1()        ) n = ZH_EVP_CIPHER_AES_256_CFB1;
   else if( p == EVP_aes_256_cfb8()        ) n = ZH_EVP_CIPHER_AES_256_CFB8;
   else if( p == EVP_aes_256_cfb128()      ) n = ZH_EVP_CIPHER_AES_256_CFB128;
#endif
   else if( p == EVP_aes_256_cfb()         ) n = ZH_EVP_CIPHER_AES_256_CFB;
   else if( p == EVP_aes_256_ofb()         ) n = ZH_EVP_CIPHER_AES_256_OFB;
#endif
#ifndef OPENSSL_NO_CAMELLIA
   else if( p == EVP_camellia_128_ecb()    ) n = ZH_EVP_CIPHER_CAMELLIA_128_ECB;
   else if( p == EVP_camellia_128_cbc()    ) n = ZH_EVP_CIPHER_CAMELLIA_128_CBC;
   else if( p == EVP_camellia_128_cfb1()   ) n = ZH_EVP_CIPHER_CAMELLIA_128_CFB1;
   else if( p == EVP_camellia_128_cfb8()   ) n = ZH_EVP_CIPHER_CAMELLIA_128_CFB8;
   else if( p == EVP_camellia_128_cfb128() ) n = ZH_EVP_CIPHER_CAMELLIA_128_CFB128;
   else if( p == EVP_camellia_128_cfb()    ) n = ZH_EVP_CIPHER_CAMELLIA_128_CFB;
   else if( p == EVP_camellia_128_ofb()    ) n = ZH_EVP_CIPHER_CAMELLIA_128_OFB;
   else if( p == EVP_camellia_192_ecb()    ) n = ZH_EVP_CIPHER_CAMELLIA_192_ECB;
   else if( p == EVP_camellia_192_cbc()    ) n = ZH_EVP_CIPHER_CAMELLIA_192_CBC;
   else if( p == EVP_camellia_192_cfb1()   ) n = ZH_EVP_CIPHER_CAMELLIA_192_CFB1;
   else if( p == EVP_camellia_192_cfb8()   ) n = ZH_EVP_CIPHER_CAMELLIA_192_CFB8;
   else if( p == EVP_camellia_192_cfb128() ) n = ZH_EVP_CIPHER_CAMELLIA_192_CFB128;
   else if( p == EVP_camellia_192_cfb()    ) n = ZH_EVP_CIPHER_CAMELLIA_192_CFB;
   else if( p == EVP_camellia_192_ofb()    ) n = ZH_EVP_CIPHER_CAMELLIA_192_OFB;
   else if( p == EVP_camellia_256_ecb()    ) n = ZH_EVP_CIPHER_CAMELLIA_256_ECB;
   else if( p == EVP_camellia_256_cbc()    ) n = ZH_EVP_CIPHER_CAMELLIA_256_CBC;
   else if( p == EVP_camellia_256_cfb1()   ) n = ZH_EVP_CIPHER_CAMELLIA_256_CFB1;
   else if( p == EVP_camellia_256_cfb8()   ) n = ZH_EVP_CIPHER_CAMELLIA_256_CFB8;
   else if( p == EVP_camellia_256_cfb128() ) n = ZH_EVP_CIPHER_CAMELLIA_256_CFB128;
   else if( p == EVP_camellia_256_cfb()    ) n = ZH_EVP_CIPHER_CAMELLIA_256_CFB;
   else if( p == EVP_camellia_256_ofb()    ) n = ZH_EVP_CIPHER_CAMELLIA_256_OFB;
#endif
   else                                      n = ZH_EVP_CIPHER_UNSUPPORTED;

   return n;
}

ZH_FUNC( EVP_GET_CIPHERBYNAME )
{
   if( ZH_ISCHAR( 1 ) )
      zh_retni( zh_EVP_CIPHER_ptr_to_id( EVP_get_cipherbyname( zh_parc( 1 ) ) ) );
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( EVP_GET_CIPHERBYNID )
{
   if( ZH_IS_PARAM_NUM( 1 ) )
      zh_retni( zh_EVP_CIPHER_ptr_to_id( EVP_get_cipherbynid( zh_parni( 1 ) ) ) );
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( EVP_CIPHER_NID )
{
   const EVP_CIPHER * cipher = zh_EVP_CIPHER_par( 1 );

   zh_retni( cipher ? EVP_CIPHER_nid( cipher ) : 0 );
}

ZH_FUNC( EVP_CIPHER_BLOCK_SIZE )
{
   const EVP_CIPHER * cipher = zh_EVP_CIPHER_par( 1 );

   zh_retni( cipher ? EVP_CIPHER_block_size( cipher ) : 0 );
}

ZH_FUNC( EVP_CIPHER_KEY_LENGTH )
{
   const EVP_CIPHER * cipher = zh_EVP_CIPHER_par( 1 );

   zh_retni( cipher ? EVP_CIPHER_key_length( cipher ) : 0 );
}

ZH_FUNC( EVP_CIPHER_IV_LENGTH )
{
   const EVP_CIPHER * cipher = zh_EVP_CIPHER_par( 1 );

   zh_retni( cipher ? EVP_CIPHER_iv_length( cipher ) : 0 );
}

ZH_FUNC( EVP_CIPHER_FLAGS )
{
   const EVP_CIPHER * cipher = zh_EVP_CIPHER_par( 1 );

   zh_retnint( cipher ? EVP_CIPHER_flags( cipher ) : 0 );
}

ZH_FUNC( EVP_CIPHER_MODE )
{
   const EVP_CIPHER * cipher = zh_EVP_CIPHER_par( 1 );

#if OPENSSL_VERSION_NUMBER < 0x00906040L
   /* fix for typo in macro definition in openssl/evp.h */
   #undef EVP_CIPHER_mode
   #define EVP_CIPHER_mode( e )  ( ( e )->flags & EVP_CIPH_MODE )
#endif
   zh_retni( cipher ? EVP_CIPHER_mode( cipher ) : 0 );
}

ZH_FUNC( EVP_CIPHER_TYPE )
{
   const EVP_CIPHER * cipher = zh_EVP_CIPHER_par( 1 );

   zh_retni( cipher ? EVP_CIPHER_type( cipher ) : 0 );
}

ZH_FUNC( EVP_CIPHER_CTX_NEW )
{
   void ** ph = ( void ** ) zh_gcAllocate( sizeof( EVP_CIPHER_CTX * ), &s_gcEVP_CIPHER_CTX_funcs );
   EVP_CIPHER_CTX * ctx;

#if OPENSSL_VERSION_NUMBER >= 0x10100000L
   ctx = EVP_CIPHER_CTX_new();
#else
   ctx = ( EVP_CIPHER_CTX * ) zh_xgrab( sizeof( EVP_CIPHER_CTX ) );
   EVP_CIPHER_CTX_init( ctx );
#endif

   *ph = ctx;

   zh_retptrGC( ph );
}


ZH_FUNC( EVP_CIPHER_CTX_RESET )
{
   if( zh_EVP_CIPHER_CTX_is( 1 ) )
   {
      EVP_CIPHER_CTX * ctx = zh_EVP_CIPHER_CTX_par( 1 );

      if( ctx )
#if OPENSSL_VERSION_NUMBER >= 0x10100000L && \
    ! defined( LIBRESSL_VERSION_NUMBER )
         zh_retni( EVP_CIPHER_CTX_reset( ctx ) );
#else
         zh_retni( EVP_CIPHER_CTX_cleanup( ctx ) );
#endif
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}


ZH_FUNC( EVP_CIPHER_CTX_SET_PADDING )
{
   if( zh_EVP_CIPHER_CTX_is( 1 ) )
   {
      EVP_CIPHER_CTX * ctx = zh_EVP_CIPHER_CTX_par( 1 );

      if( ctx )
      {
#if OPENSSL_VERSION_NUMBER >= 0x00907000L
         zh_retni( EVP_CIPHER_CTX_set_padding( ctx, zh_parni( 2 ) ) );
#else
         zh_retni( 0 );
#endif
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( EVP_CIPHER_CTX_KEY_LENGTH )
{
   if( zh_EVP_CIPHER_CTX_is( 1 ) )
   {
      EVP_CIPHER_CTX * ctx = zh_EVP_CIPHER_CTX_par( 1 );

      if( ctx )
         zh_retni( EVP_CIPHER_CTX_key_length( ctx ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( EVP_CIPHER_CTX_SET_KEY_LENGTH )
{
   if( zh_EVP_CIPHER_CTX_is( 1 ) )
   {
      EVP_CIPHER_CTX * ctx = zh_EVP_CIPHER_CTX_par( 1 );

      if( ctx )
         zh_retni( EVP_CIPHER_CTX_set_key_length( ctx, zh_parni( 2 ) ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( EVP_CIPHER_CTX_CTRL )
{
   if( zh_EVP_CIPHER_CTX_is( 1 ) )
   {
      EVP_CIPHER_CTX * ctx = zh_EVP_CIPHER_CTX_par( 1 );

      if( ctx )
         /* NOTE: 4th param doesn't have a 'const' qualifier. This is a setter
                  function, so even if we do a copy, what sort of allocation
                  routine to use? [vszakats] */
         zh_retni( EVP_CIPHER_CTX_ctrl( ctx, zh_parni( 2 ), zh_parni( 3 ), ( void * ) ZH_UNCONST( zh_parc( 4 ) ) ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( EVP_CIPHER_CTX_CIPHER )
{
   if( zh_EVP_CIPHER_CTX_is( 1 ) )
   {
      EVP_CIPHER_CTX * ctx = zh_EVP_CIPHER_CTX_par( 1 );

      if( ctx )
         zh_retni( zh_EVP_CIPHER_ptr_to_id( EVP_CIPHER_CTX_cipher( ctx ) ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( EVP_ENCRYPTINIT )
{
   const EVP_CIPHER * cipher = zh_EVP_CIPHER_par( 2 );

   if( zh_EVP_CIPHER_CTX_is( 1 ) && cipher )
   {
      EVP_CIPHER_CTX * ctx = zh_EVP_CIPHER_CTX_par( 1 );

      if( ctx )
         zh_retni( EVP_EncryptInit( ctx,
                                    cipher,
                                    ( ZH_SSL_CONST unsigned char * ) zh_parc( 3 ),
                                    ( ZH_SSL_CONST unsigned char * ) zh_parc( 4 ) ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( EVP_ENCRYPTINIT_EX )
{
   const EVP_CIPHER * cipher = zh_EVP_CIPHER_par( 2 );

   if( zh_EVP_CIPHER_CTX_is( 1 ) && cipher )
   {
      EVP_CIPHER_CTX * ctx = zh_EVP_CIPHER_CTX_par( 1 );

      if( ctx )
      {
#if OPENSSL_VERSION_NUMBER >= 0x00907000L
         zh_retni( EVP_EncryptInit_ex( ctx,
                                       cipher,
                                       ( ENGINE * ) zh_parptr( 3 ),
                                       ( const unsigned char * ) zh_parc( 4 ),
                                       ( const unsigned char * ) zh_parc( 5 ) ) );
#else
         zh_retni( 0 );
#endif
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( EVP_ENCRYPTUPDATE )
{
   if( zh_EVP_CIPHER_CTX_is( 1 ) )
   {
      EVP_CIPHER_CTX * ctx = zh_EVP_CIPHER_CTX_par( 1 );

      if( ctx )
      {
         int size = ( int ) zh_parclen( 3 ) + EVP_CIPHER_CTX_block_size( ctx ) - 1;
         unsigned char * buffer = ( unsigned char * ) zh_xgrab( size + 1 );

         zh_retni( EVP_EncryptUpdate( ctx,
                                      buffer,
                                      &size,
                                      ( ZH_SSL_CONST unsigned char * ) zh_parcx( 3 ),
                                      ( int ) zh_parclen( 3 ) ) );

         if( size > 0 )
         {
            if( ! zh_storclen_buffer( ( char * ) buffer, size, 2 ) )
               zh_xfree( buffer );
         }
         else
         {
            zh_xfree( buffer );
            zh_storc( NULL, 2 );
         }
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( EVP_ENCRYPTFINAL )
{
   if( zh_EVP_CIPHER_CTX_is( 1 ) )
   {
      EVP_CIPHER_CTX * ctx = zh_EVP_CIPHER_CTX_par( 1 );

      if( ctx )
      {
         int size = EVP_CIPHER_CTX_block_size( ctx );
         unsigned char * buffer = ( unsigned char * ) zh_xgrab( size + 1 );

         zh_retni( EVP_EncryptFinal( ctx, buffer, &size ) );

         if( size > 0 )
         {
            if( ! zh_storclen_buffer( ( char * ) buffer, size, 2 ) )
               zh_xfree( buffer );
         }
         else
         {
            zh_xfree( buffer );
            zh_storc( NULL, 2 );
         }
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( EVP_ENCRYPTFINAL_EX )
{
   if( zh_EVP_CIPHER_CTX_is( 1 ) )
   {
      EVP_CIPHER_CTX * ctx = zh_EVP_CIPHER_CTX_par( 1 );

      if( ctx )
      {
#if OPENSSL_VERSION_NUMBER >= 0x00907000L
         int size = EVP_CIPHER_CTX_block_size( ctx );
         unsigned char * buffer = ( unsigned char * ) zh_xgrab( size + 1 );

         zh_retni( EVP_EncryptFinal_ex( ctx, buffer, &size ) );

         if( size > 0 )
         {
            if( ! zh_storclen_buffer( ( char * ) buffer, size, 2 ) )
               zh_xfree( buffer );
         }
         else
         {
            zh_xfree( buffer );
            zh_storc( NULL, 2 );
         }
#else
         zh_retni( 0 );
         zh_storc( NULL, 2 );
#endif
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( EVP_DECRYPTINIT )
{
   const EVP_CIPHER * cipher = zh_EVP_CIPHER_par( 2 );

   if( zh_EVP_CIPHER_CTX_is( 1 ) && cipher )
   {
      EVP_CIPHER_CTX * ctx = zh_EVP_CIPHER_CTX_par( 1 );

      if( ctx )
         zh_retni( EVP_DecryptInit( ctx,
                                    cipher,
                                    ( ZH_SSL_CONST unsigned char * ) zh_parc( 3 ),
                                    ( ZH_SSL_CONST unsigned char * ) zh_parc( 4 ) ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( EVP_DECRYPTINIT_EX )
{
   const EVP_CIPHER * cipher = zh_EVP_CIPHER_par( 2 );

   if( zh_EVP_CIPHER_CTX_is( 1 ) && cipher )
   {
      EVP_CIPHER_CTX * ctx = zh_EVP_CIPHER_CTX_par( 1 );

      if( ctx )
      {
#if OPENSSL_VERSION_NUMBER >= 0x00907000L
         zh_retni( EVP_DecryptInit_ex( ctx,
                                       cipher,
                                       ( ENGINE * ) zh_parptr( 3 ),
                                       ( const unsigned char * ) zh_parc( 4 ),
                                       ( const unsigned char * ) zh_parc( 5 ) ) );
#else
         zh_retni( 0 );
#endif
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( EVP_DECRYPTUPDATE )
{
   if( zh_EVP_CIPHER_CTX_is( 1 ) )
   {
      EVP_CIPHER_CTX * ctx = zh_EVP_CIPHER_CTX_par( 1 );

      if( ctx )
      {
         int size = ( int ) zh_parclen( 3 ) + EVP_CIPHER_CTX_block_size( ctx );
         unsigned char * buffer = ( unsigned char * ) zh_xgrab( size + 1 );

         zh_retni( EVP_DecryptUpdate( ctx,
                                      buffer,
                                      &size,
                                      ( ZH_SSL_CONST unsigned char * ) zh_parcx( 3 ),
                                      ( int ) zh_parclen( 3 ) ) );

         if( size > 0 )
         {
            if( ! zh_storclen_buffer( ( char * ) buffer, size, 2 ) )
               zh_xfree( buffer );
         }
         else
         {
            zh_xfree( buffer );
            zh_storc( NULL, 2 );
         }
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( EVP_DECRYPTFINAL )
{
   if( zh_EVP_CIPHER_CTX_is( 1 ) )
   {
      EVP_CIPHER_CTX * ctx = zh_EVP_CIPHER_CTX_par( 1 );

      if( ctx )
      {
         int size = EVP_CIPHER_CTX_block_size( ctx );
         unsigned char * buffer = ( unsigned char * ) zh_xgrab( size + 1 );

         zh_retni( EVP_DecryptFinal( ctx, buffer, &size ) );

         if( size > 0 )
         {
            if( ! zh_storclen_buffer( ( char * ) buffer, size, 2 ) )
               zh_xfree( buffer );
         }
         else
         {
            zh_xfree( buffer );
            zh_storc( NULL, 2 );
         }
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( EVP_DECRYPTFINAL_EX )
{
   if( zh_EVP_CIPHER_CTX_is( 1 ) )
   {
      EVP_CIPHER_CTX * ctx = zh_EVP_CIPHER_CTX_par( 1 );

      if( ctx )
      {
#if OPENSSL_VERSION_NUMBER >= 0x00907000L
         int size = EVP_CIPHER_CTX_block_size( ctx );
         unsigned char * buffer = ( unsigned char * ) zh_xgrab( size + 1 );

         zh_retni( EVP_DecryptFinal_ex( ctx, buffer, &size ) );

         if( size > 0 )
         {
            if( ! zh_storclen_buffer( ( char * ) buffer, size, 2 ) )
               zh_xfree( buffer );
         }
         else
         {
            zh_xfree( buffer );
            zh_storc( NULL, 2 );
         }
#else
         zh_retni( 0 );
         zh_storc( NULL, 2 );
#endif
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( EVP_CIPHERINIT )
{
   const EVP_CIPHER * cipher = zh_EVP_CIPHER_par( 2 );

   if( zh_EVP_CIPHER_CTX_is( 1 ) && cipher )
   {
      EVP_CIPHER_CTX * ctx = zh_EVP_CIPHER_CTX_par( 1 );

      if( ctx )
         zh_retni( EVP_CipherInit( ctx,
                                   cipher,
                                   ( ZH_SSL_CONST unsigned char * ) zh_parc( 3 ),
                                   ( ZH_SSL_CONST unsigned char * ) zh_parc( 4 ),
                                   zh_parni( 5 ) ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( EVP_CIPHERINIT_EX )
{
   const EVP_CIPHER * cipher = zh_EVP_CIPHER_par( 2 );

   if( zh_EVP_CIPHER_CTX_is( 1 ) && cipher )
   {
      EVP_CIPHER_CTX * ctx = zh_EVP_CIPHER_CTX_par( 1 );

      if( ctx )
      {
#if OPENSSL_VERSION_NUMBER >= 0x00907000L
         zh_retni( EVP_CipherInit_ex( ctx,
                                      cipher,
                                      ( ENGINE * ) zh_parptr( 3 ),
                                      ( const unsigned char * ) zh_parc( 4 ),
                                      ( const unsigned char * ) zh_parc( 5 ),
                                      zh_parni( 6 ) ) );
#else
         zh_retni( 0 );
#endif
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( EVP_CIPHERUPDATE )
{
   if( zh_EVP_CIPHER_CTX_is( 1 ) )
   {
      EVP_CIPHER_CTX * ctx = zh_EVP_CIPHER_CTX_par( 1 );

      if( ctx )
      {
         int size = ( int ) zh_parclen( 3 ) + EVP_CIPHER_CTX_block_size( ctx ) - 1;
         unsigned char * buffer = ( unsigned char * ) zh_xgrab( size + 1 );

         zh_retni( EVP_CipherUpdate( ctx,
                                     buffer,
                                     &size,
                                     ( ZH_SSL_CONST unsigned char * ) zh_parcx( 3 ),
                                     ( int ) zh_parclen( 3 ) ) );

         if( size > 0 )
         {
            if( ! zh_storclen_buffer( ( char * ) buffer, size, 2 ) )
               zh_xfree( buffer );
         }
         else
         {
            zh_xfree( buffer );
            zh_storc( NULL, 2 );
         }
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( EVP_CIPHERFINAL )
{
   if( zh_EVP_CIPHER_CTX_is( 1 ) )
   {
      EVP_CIPHER_CTX * ctx = zh_EVP_CIPHER_CTX_par( 1 );

      if( ctx )
      {
         int size = EVP_CIPHER_CTX_block_size( ctx );
         unsigned char * buffer = ( unsigned char * ) zh_xgrab( size + 1 );

         zh_retni( EVP_CipherFinal( ctx, buffer, &size ) );

         if( size > 0 )
         {
            if( ! zh_storclen_buffer( ( char * ) buffer, size, 2 ) )
               zh_xfree( buffer );
         }
         else
         {
            zh_xfree( buffer );
            zh_storc( NULL, 2 );
         }
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( EVP_CIPHERFINAL_EX )
{
   if( zh_EVP_CIPHER_CTX_is( 1 ) )
   {
      EVP_CIPHER_CTX * ctx = zh_EVP_CIPHER_CTX_par( 1 );

      if( ctx )
      {
#if OPENSSL_VERSION_NUMBER >= 0x00907000L
         int size = EVP_CIPHER_CTX_block_size( ctx );
         unsigned char * buffer = ( unsigned char * ) zh_xgrab( size + 1 );

         zh_retni( EVP_CipherFinal_ex( ctx, buffer, &size ) );

         if( size > 0 )
         {
            if( ! zh_storclen_buffer( ( char * ) buffer, size, 2 ) )
               zh_xfree( buffer );
         }
         else
         {
            zh_xfree( buffer );
            zh_storc( NULL, 2 );
         }
#else
         zh_retni( 0 );
         zh_storc( NULL, 2 );
#endif
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( EVP_SEALINIT )
{
   const EVP_CIPHER * cipher = zh_EVP_CIPHER_par( 2 );

   if( zh_EVP_CIPHER_CTX_is( 1 ) && cipher )
   {
      EVP_CIPHER_CTX * ctx = zh_EVP_CIPHER_CTX_par( 1 );

      if( ctx )
      {
         int        npubk  = 0;
         PZH_ITEM   pArray = NULL;
         EVP_PKEY * pkey1  = NULL;

         if( ZH_ISARRAY( 5 ) )
            npubk = ( int ) zh_arrayLen( pArray = zh_param( 5, ZH_IT_ARRAY ) );
         else if( ZH_ISPOINTER( 5 ) )
         {
            if( ( pkey1 = ( EVP_PKEY * ) zh_parptr( 5 ) ) != NULL )
               npubk = 1;
         }

         if( npubk > 0 )
         {
            unsigned char ** ek = ( unsigned char ** ) zh_xgrab( sizeof( unsigned char * ) * npubk );
            int * ekl = ( int * ) zh_xgrab( sizeof( int ) * npubk );
            int   ivl = EVP_CIPHER_iv_length( cipher );
            unsigned char * iv = ivl > 0 ? ( unsigned char * ) zh_xgrab( ivl + 1 ) : NULL;

            EVP_PKEY ** pubk = ( EVP_PKEY ** ) zh_xgrab( sizeof( EVP_PKEY * ) * npubk + 1 );
            PZH_ITEM    pPKEY;
            int         tmp;

            for( tmp = 0; tmp < npubk; tmp++ )
            {
               pubk[ tmp ] = pkey1 ? pkey1 : ( EVP_PKEY * ) zh_arrayGetPtr( pArray, tmp + 1 );
               ek[ tmp ]   = ( unsigned char * ) zh_xgrab( EVP_PKEY_size( pubk[ tmp ] ) + 1 );
               ekl[ tmp ]  = 0;
            }

            zh_retni( EVP_SealInit( ctx,
                                    ( ZH_SSL_CONST EVP_CIPHER * ) cipher,
                                    ek,
                                    ekl,
                                    iv,
                                    pubk,
                                    npubk ) );

            pPKEY = zh_itemArrayNew( npubk );

            for( tmp = 0; tmp < npubk; tmp++ )
               zh_arraySetCLPtr( pPKEY, tmp + 1, ( char * ) ek[ tmp ], ekl[ tmp ] );

            zh_itemParamStoreForward( 3, pPKEY );
            zh_itemRelease( pPKEY );

            if( iv )
            {
               if( ! zh_storclen_buffer( ( char * ) iv, ivl, 4 ) )
                  zh_xfree( iv );
            }
            else
               zh_stor( 4 );

            zh_xfree( ek );
            zh_xfree( ekl );
            zh_xfree( pubk );
         }
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( EVP_SEALUPDATE )
{
   if( zh_EVP_CIPHER_CTX_is( 1 ) )
   {
      EVP_CIPHER_CTX * ctx = zh_EVP_CIPHER_CTX_par( 1 );

      if( ctx )
      {
         int size = ( int ) zh_parclen( 3 ) + EVP_CIPHER_CTX_block_size( ctx ) - 1;
         unsigned char * buffer = ( unsigned char * ) zh_xgrab( size + 1 );

         zh_retni( EVP_SealUpdate( ctx,
                                   buffer,
                                   &size,
                                   ( ZH_SSL_CONST unsigned char * ) zh_parcx( 3 ),
                                   ( int ) zh_parclen( 3 ) ) );

         if( size > 0 )
         {
            if( ! zh_storclen_buffer( ( char * ) buffer, size, 2 ) )
               zh_xfree( buffer );
         }
         else
         {
            zh_xfree( buffer );
            zh_storc( NULL, 2 );
         }
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( EVP_SEALFINAL )
{
   if( zh_EVP_CIPHER_CTX_is( 1 ) )
   {
      EVP_CIPHER_CTX * ctx = zh_EVP_CIPHER_CTX_par( 1 );

      if( ctx )
      {
         int size = EVP_CIPHER_CTX_block_size( ctx );
         unsigned char * buffer = ( unsigned char * ) zh_xgrab( size + 1 );

#if OPENSSL_VERSION_NUMBER >= 0x00907000L
         zh_retni( EVP_SealFinal( ctx, buffer, &size ) );
#else
         EVP_SealFinal( ctx, buffer, &size );
         zh_retni( 1 );
#endif

         if( size > 0 )
         {
            if( ! zh_storclen_buffer( ( char * ) buffer, size, 2 ) )
               zh_xfree( buffer );
         }
         else
         {
            zh_xfree( buffer );
            zh_storc( NULL, 2 );
         }
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( EVP_OPENINIT )
{
   const EVP_CIPHER * cipher = zh_EVP_CIPHER_par( 2 );

   if( zh_EVP_CIPHER_CTX_is( 1 ) && cipher )
   {
      EVP_CIPHER_CTX * ctx  = zh_EVP_CIPHER_CTX_par( 1 );
      EVP_PKEY *       priv = ( EVP_PKEY * ) zh_parptr( 5 );

      if( ctx && priv )
         zh_retni( EVP_OpenInit( ctx,
                                 ( ZH_SSL_CONST EVP_CIPHER * ) cipher,
                                 ( ZH_SSL_CONST unsigned char * ) zh_parcx( 3 ),
                                 ( int ) zh_parclen( 3 ),
                                 ( ZH_ISCHAR( 4 ) && ( int ) zh_parclen( 4 ) == EVP_CIPHER_iv_length( cipher ) ) ? ( ZH_SSL_CONST unsigned char * ) zh_parc( 4 ) : NULL,
                                 priv ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( EVP_OPENUPDATE )
{
   if( zh_EVP_CIPHER_CTX_is( 1 ) )
   {
      EVP_CIPHER_CTX * ctx = zh_EVP_CIPHER_CTX_par( 1 );

      if( ctx )
      {
         int size = ( int ) zh_parclen( 3 ) + EVP_CIPHER_CTX_block_size( ctx ) - 1;
         unsigned char * buffer = ( unsigned char * ) zh_xgrab( size + 1 );

         zh_retni( EVP_OpenUpdate( ctx,
                                   buffer,
                                   &size,
                                   ( ZH_SSL_CONST unsigned char * ) zh_parcx( 3 ),
                                   ( int ) zh_parclen( 3 ) ) );

         if( size > 0 )
         {
            if( ! zh_storclen_buffer( ( char * ) buffer, size, 2 ) )
               zh_xfree( buffer );
         }
         else
         {
            zh_xfree( buffer );
            zh_storc( NULL, 2 );
         }
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( EVP_OPENFINAL )
{
   if( zh_EVP_CIPHER_CTX_is( 1 ) )
   {
      EVP_CIPHER_CTX * ctx = zh_EVP_CIPHER_CTX_par( 1 );

      if( ctx )
      {
         int size = EVP_CIPHER_CTX_block_size( ctx );
         unsigned char * buffer = ( unsigned char * ) zh_xgrab( size + 1 );

         zh_retni( EVP_OpenFinal( ctx, buffer, &size ) );

         if( size > 0 )
         {
            if( ! zh_storclen_buffer( ( char * ) buffer, size, 2 ) )
               zh_xfree( buffer );
         }
         else
         {
            zh_xfree( buffer );
            zh_storc( NULL, 2 );
         }
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

#if 0

void * EVP_CIPHER_CTX_get_app_data( const EVP_CIPHER_CTX * ctx );
void EVP_CIPHER_CTX_set_app_data( EVP_CIPHER_CTX * ctx, void * data );
int EVP_CIPHER_param_to_asn1( EVP_CIPHER_CTX * ctx, ASN1_TYPE * type );
int EVP_CIPHER_asn1_to_param( EVP_CIPHER_CTX * ctx, ASN1_TYPE * type );

#endif