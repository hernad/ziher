/*
 * Default RDD module
 *
 * Copyright 1999 Bruno Cantero <bruno@issnet.net>
 * Copyright 2004-2007 Przemyslaw Czerpak
 * Copyright 2002 Horacio Roldan <ziher_ar@yahoo.com.ar> (zh_waCloseAux())
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

#include "zh_api.h"
#include "zh_rdd_api.h"
#include "zh_item_api.h"
#include "zh_api_error.h"
#include "zh_lang_api.h"
#include "zh_vm.h"
#include "zh_thread.h"
#include "zh_set.h"

/*
 * -- BASIC RDD METHODS --
 */

/*
 * Determine logical beginning of file.
 */
static ZH_ERRCODE zh_waBof( AREAP pArea, ZH_BOOL * pBof )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_waBof(%p, %p)", ( void * ) pArea, ( void * ) pBof ) );

   *pBof = pArea->fBof;
   return ZH_SUCCESS;
}

/*
 * Determine logical end of file.
 */
static ZH_ERRCODE zh_waEof( AREAP pArea, ZH_BOOL * pEof )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_waEof(%p, %p)", ( void * ) pArea, ( void * ) pEof ) );

   *pEof = pArea->fEof;
   return ZH_SUCCESS;
}

/*
 * Determine outcome of the last search operation.
 */
static ZH_ERRCODE zh_waFound( AREAP pArea, ZH_BOOL * pFound )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_waFound(%p, %p)", ( void * ) pArea, ( void * ) pFound ) );

   *pFound = pArea->fFound;
   return ZH_SUCCESS;
}

/*
 * Reposition cursor relative to current position.
 */
static ZH_ERRCODE zh_waSkip( AREAP pArea, ZH_LONG lToSkip )
{
   ZH_LONG lSkip;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_waSkip(%p, %ld)", ( void * ) pArea, lToSkip ) );

   /* Flush record and exit */
   if( lToSkip == 0 )
      return SELF_SKIPRAW( pArea, 0 );

   pArea->fTop = pArea->fBottom = ZH_FALSE;

   if( lToSkip > 0 )
      lSkip = 1;
   else
   {
      lSkip = -1;
      lToSkip *= -1;
   }
   while( --lToSkip >= 0 )
   {
      if( SELF_SKIPRAW( pArea, lSkip ) != ZH_SUCCESS )
         return ZH_FAILURE;
      if( SELF_SKIPFILTER( pArea, lSkip ) != ZH_SUCCESS )
         return ZH_FAILURE;
      if( pArea->fBof || pArea->fEof )
         break;
   }

   /* Update Bof and Eof flags */
   if( lSkip < 0 )
      pArea->fEof = ZH_FALSE;
   else /* ( lSkip > 0 ) */
      pArea->fBof = ZH_FALSE;

   return ZH_SUCCESS;
}

/*
 * Reposition cursor respecting any filter setting.
 */
static ZH_ERRCODE zh_waSkipFilter( AREAP pArea, ZH_LONG lUpDown )
{
   ZH_BOOL fBottom, fDeleted;
   ZH_ERRCODE errCode;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_waSkipFilter(%p, %ld)", ( void * ) pArea, lUpDown ) );

   if( pArea->dbfi.itmCobExpr == NULL && ! zh_setGetDeleted() )
      return ZH_SUCCESS;

   /* Since lToSkip is passed to SkipRaw, it should never request more than
      a single skip.
      The implied purpose of zh_waSkipFilter() is to get off of a "bad" record
      after a skip was performed, NOT to skip lToSkip filtered records.
    */
   lUpDown = ( lUpDown < 0  ? -1 : 1 );

   /* remember if we are here after SLEF_GOTOP() */
   fBottom = pArea->fBottom;

   while( ! pArea->fBof && ! pArea->fEof )
   {
      /* SET DELETED */
      if( zh_setGetDeleted() )
      {
         if( SELF_DELETED( pArea, &fDeleted ) != ZH_SUCCESS )
            return ZH_FAILURE;
         if( fDeleted )
         {
            if( SELF_SKIPRAW( pArea, lUpDown ) != ZH_SUCCESS )
               return ZH_FAILURE;
            continue;
         }
      }

      /* SET FILTER TO */
      if( pArea->dbfi.itmCobExpr )
      {
         if( SELF_EVALBLOCK( pArea, pArea->dbfi.itmCobExpr ) != ZH_SUCCESS )
            return ZH_FAILURE;

         if( ZH_IS_LOGICAL( pArea->valResult ) &&
             ! zh_itemGetL( pArea->valResult ) )
         {
            if( SELF_SKIPRAW( pArea, lUpDown ) != ZH_SUCCESS )
               return ZH_FAILURE;
            continue;
         }
      }

      break;
   }

   /*
    * The only one situation when we should repos is backward skipping
    * if we are at BOTTOM position (it's SKIPFILTER called from GOBOTTOM)
    * then GOEOF() if not then GOTOP()
    */
   if( pArea->fBof && lUpDown < 0 )
   {
      if( fBottom )
      {
         /* GOTO EOF (phantom) record -
            this is the only one place where GOTO is used by Ziher
            directly and RDD which does not operate on numbers should
            serve this method only as SELF_GOEOF() synonym. If it's a
            problem then we can remove this if and always use SELF_GOTOP()
            but it also means second table scan if all records filtered
            are out of filter so I do not want to do that. I will prefer
            explicit add SELF_GOEOF() method
          */
         errCode = SELF_GOTO( pArea, 0 );
      }
      else
      {
         errCode = SELF_GOTOP( pArea );
         pArea->fBof = ZH_TRUE;
      }
   }
   else
   {
      errCode = ZH_SUCCESS;
   }

   return errCode;
}

/*
 * Add a field to the WorkArea.
 */
static ZH_ERRCODE zh_waAddField( AREAP pArea, LPDBFIELDINFO pFieldInfo )
{
   LPFIELD pField;
   char szFieldName[ ZH_SYMBOL_NAME_LEN + 1 ];
   const char *szPtr;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_waAddField(%p, %p)", ( void * ) pArea, ( void * ) pFieldInfo ) );

   /* Validate the name of field */
   szPtr = pFieldInfo->atomName;
   while( ZH_ISSPACE( *szPtr ) )
      ++szPtr;
   zh_strncpyUpperTrim( szFieldName, szPtr,
                        ZH_MIN( ZH_SYMBOL_NAME_LEN, pArea->uiMaxFieldNameLength ) );
   if( szFieldName[ 0 ] == 0 )
      return ZH_FAILURE;

   pField = pArea->lpFields + pArea->uiFieldCount;
   if( pArea->uiFieldCount > 0 )
      ( ( LPFIELD ) ( pField - 1 ) )->lpfNext = pField;
   pField->sym = ( void * ) zh_dynsymGetCase( szFieldName );
   pField->uiType = pFieldInfo->uiType;
   pField->uiTypeExtended = pFieldInfo->uiTypeExtended;
   pField->uiLen = pFieldInfo->uiLen;
   pField->uiDec = pFieldInfo->uiDec;
   pField->uiFlags = pFieldInfo->uiFlags;
   pField->uiArea = pArea->uiArea;
   pArea->uiFieldCount++;

   return ZH_SUCCESS;
}

/*
 * Add all fields defined in an array to the WorkArea.
 */
static ZH_ERRCODE zh_waCreateFields( AREAP pArea, PZH_ITEM pStruct )
{
   ZH_USHORT uiItems, uiCount;
   ZH_ERRCODE errCode = ZH_SUCCESS;
   DBFIELDINFO dbFieldInfo;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_waCreateFields(%p, %p)", ( void * ) pArea, ( void * ) pStruct ) );

   uiItems = ( ZH_USHORT ) zh_arrayLen( pStruct );
   if( SELF_SETFIELDEXTENT( pArea, uiItems ) != ZH_SUCCESS )
      return ZH_FAILURE;

   for( uiCount = 0; uiCount < uiItems; uiCount++ )
   {
      ZH_USHORT uiLen, uiDec;
      PZH_ITEM pFieldDesc;
      const char * szType;
      int iData;

      dbFieldInfo.uiTypeExtended = 0;
      pFieldDesc = zh_arrayGetItemPtr( pStruct, uiCount + 1 );
      dbFieldInfo.atomName = zh_arrayGetCPtr( pFieldDesc, DBS_NAME );
      iData = zh_arrayGetNI( pFieldDesc, DBS_LEN );
      if( iData < 0 )
         iData = 0;
      uiLen = dbFieldInfo.uiLen = ( ZH_USHORT ) iData;
      iData = zh_arrayGetNI( pFieldDesc, DBS_DEC );
      if( iData < 0 )
         iData = 0;
      uiDec = ( ZH_USHORT ) iData;
      dbFieldInfo.uiDec = 0;
      szType = zh_arrayGetCPtr( pFieldDesc, DBS_TYPE );
      iData = ZH_TOUPPER( *szType );

      dbFieldInfo.uiFlags = 0;
      while( *++szType )
      {
         if( *szType == ':' )
         {
            while( *++szType )
            {
               switch( ZH_TOUPPER( *szType ) )
               {
                  case 'N':
                     dbFieldInfo.uiFlags |= ZH_FF_NULLABLE;
                     break;
                  case 'B':
                     dbFieldInfo.uiFlags |= ZH_FF_BINARY;
                     break;
                  case '+':
                     dbFieldInfo.uiFlags |= ZH_FF_AUTOINC;
                     break;
                  case 'Z':
                     dbFieldInfo.uiFlags |= ZH_FF_COMPRESSED;
                     break;
                  case 'E':
                     dbFieldInfo.uiFlags |= ZH_FF_ENCRYPTED;
                     break;
                  case 'U':
                     dbFieldInfo.uiFlags |= ZH_FF_UNICODE;
                     break;
               }
            }
            break;
         }
      }

      switch( iData )
      {
         case 'C':
            dbFieldInfo.uiType = ZH_FT_STRING;
            dbFieldInfo.uiLen = uiLen;
            dbFieldInfo.uiFlags &= ZH_FF_NULLABLE | ZH_FF_BINARY |
                                   ZH_FF_COMPRESSED | ZH_FF_ENCRYPTED |
                                   ZH_FF_UNICODE;
            break;

         case 'L':
            dbFieldInfo.uiType = ZH_FT_LOGICAL;
            dbFieldInfo.uiLen = 1;
            dbFieldInfo.uiFlags &= ZH_FF_NULLABLE;
            break;

         case 'D':
            dbFieldInfo.uiType = ZH_FT_DATE;
            dbFieldInfo.uiLen = ( uiLen == 3 || uiLen == 4 ) ? uiLen : 8;
            dbFieldInfo.uiFlags &= ZH_FF_NULLABLE;
            break;

         case 'I':
            dbFieldInfo.uiType = ZH_FT_INTEGER;
            dbFieldInfo.uiLen = ( ( uiLen > 0 && uiLen <= 4 ) || uiLen == 8 ) ? uiLen : 4;
            dbFieldInfo.uiDec = uiDec;
            dbFieldInfo.uiFlags &= ZH_FF_NULLABLE | ZH_FF_AUTOINC;
            break;

         case 'Y':
            dbFieldInfo.uiType = ZH_FT_CURRENCY;
            dbFieldInfo.uiLen = 8;
            dbFieldInfo.uiDec = 4;
            dbFieldInfo.uiFlags &= ZH_FF_NULLABLE;
            break;

         case 'Z':
            dbFieldInfo.uiType = ZH_FT_CURDOUBLE;
            dbFieldInfo.uiLen = 8;
            dbFieldInfo.uiDec = uiDec;
            dbFieldInfo.uiFlags &= ZH_FF_NULLABLE;
            break;

         case '2':
         case '4':
            dbFieldInfo.uiType = ZH_FT_INTEGER;
            dbFieldInfo.uiLen = ( ZH_USHORT ) ( iData - '0' );
            dbFieldInfo.uiFlags &= ZH_FF_NULLABLE | ZH_FF_AUTOINC;
            break;

         case 'B':
         case '8':
            dbFieldInfo.uiType = ZH_FT_DOUBLE;
            dbFieldInfo.uiLen = 8;
            dbFieldInfo.uiDec = uiDec;
            dbFieldInfo.uiFlags &= ZH_FF_NULLABLE | ZH_FF_AUTOINC;
            break;

         case 'N':
            dbFieldInfo.uiType = ZH_FT_LONG;
            dbFieldInfo.uiDec = uiDec;
            if( uiLen > 255 )
               errCode = ZH_FAILURE;
            dbFieldInfo.uiFlags &= ZH_FF_NULLABLE | ZH_FF_AUTOINC;
            break;

         case 'F':
            dbFieldInfo.uiType = ZH_FT_FLOAT;
            dbFieldInfo.uiDec = uiDec;
            /* see note above */
            if( uiLen > 255 )
               errCode = ZH_FAILURE;
            dbFieldInfo.uiFlags &= ZH_FF_NULLABLE | ZH_FF_AUTOINC;
            break;

         case 'T':
            if( uiLen == 8 )
            {
               dbFieldInfo.uiType = ZH_FT_TIMESTAMP;
               dbFieldInfo.uiLen = 8;
            }
            else
            {
               dbFieldInfo.uiType = ZH_FT_TIME;
               dbFieldInfo.uiLen = 4;
            }
            dbFieldInfo.uiFlags &= ZH_FF_NULLABLE;
            break;

         case '@':
            dbFieldInfo.uiType = ZH_FT_TIMESTAMP;
            dbFieldInfo.uiLen = 8;
            dbFieldInfo.uiFlags &= ZH_FF_NULLABLE;
            break;

         case '=':
            dbFieldInfo.uiType = ZH_FT_MODTIME;
            dbFieldInfo.uiLen = 8;
            dbFieldInfo.uiFlags = 0;
            break;

         case '^':
            dbFieldInfo.uiType = ZH_FT_ROWVER;
            dbFieldInfo.uiLen = 8;
            dbFieldInfo.uiFlags = 0;
            break;

         case '+':
            dbFieldInfo.uiType = ZH_FT_AUTOINC;
            dbFieldInfo.uiLen = 4;
            dbFieldInfo.uiFlags = 0;
            break;

         case 'Q':
            dbFieldInfo.uiType = ZH_FT_VARLENGTH;
            dbFieldInfo.uiLen = uiLen > 255 ? 255 : ( uiLen == 0 ? 1 : uiLen );
            dbFieldInfo.uiFlags &= ZH_FF_NULLABLE | ZH_FF_BINARY |
                                   ZH_FF_COMPRESSED | ZH_FF_ENCRYPTED |
                                   ZH_FF_UNICODE;
            break;

         case 'V':
            dbFieldInfo.uiType = ZH_FT_ANY;
            dbFieldInfo.uiLen = ( uiLen < 3 || uiLen == 5 ) ? 6 : uiLen;
            dbFieldInfo.uiFlags &= ZH_FF_NULLABLE | ZH_FF_BINARY |
                                   ZH_FF_COMPRESSED | ZH_FF_ENCRYPTED |
                                   ZH_FF_UNICODE;
            break;

         case 'M':
            dbFieldInfo.uiType = ZH_FT_MEMO;
            dbFieldInfo.uiLen = ( uiLen == 4 ) ? 4 : 10;
            dbFieldInfo.uiFlags &= ZH_FF_NULLABLE | ZH_FF_BINARY |
                                   ZH_FF_COMPRESSED | ZH_FF_ENCRYPTED |
                                   ZH_FF_UNICODE;
            break;

         case 'P':
            dbFieldInfo.uiType = ZH_FT_IMAGE;
            dbFieldInfo.uiLen = ( uiLen == 4 ) ? 4 : 10;
            dbFieldInfo.uiFlags &= ZH_FF_BINARY;
            break;

         case 'W':
            dbFieldInfo.uiType = ZH_FT_BLOB;
            dbFieldInfo.uiLen = ( uiLen == 4 ) ? 4 : 10;
            dbFieldInfo.uiFlags &= ZH_FF_BINARY;
            break;

         case 'G':
            dbFieldInfo.uiType = ZH_FT_OLE;
            dbFieldInfo.uiLen = ( uiLen == 4 ) ? 4 : 10;
            dbFieldInfo.uiFlags &= ZH_FF_BINARY;
            break;

         default:
            errCode = ZH_FAILURE;
            break;
      }

      if( errCode == ZH_SUCCESS )
         errCode = SELF_ADDFIELD( pArea, &dbFieldInfo ); /* Add field */

      if( errCode != ZH_SUCCESS )
      {
         zh_errRT_DBCMD( EG_ARG, EDBCMD_DBCMDBADPARAMETER, NULL, ZH_ERR_FUNCNAME );
         return errCode;
      }
   }
   return ZH_SUCCESS;
}

/*
 * Determine the number of fields in the WorkArea.
 */
static ZH_ERRCODE zh_waFieldCount( AREAP pArea, ZH_USHORT * uiFields )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_waFieldCount(%p, %p)", ( void * ) pArea, ( void * ) uiFields ) );

   * uiFields = pArea->uiFieldCount;
   return ZH_SUCCESS;
}

/*
 * Retrieve information about a field.
 */
static ZH_ERRCODE zh_waFieldInfo( AREAP pArea, ZH_USHORT uiIndex, ZH_USHORT uiType, PZH_ITEM pItem )
{
   LPFIELD pField;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_waFieldInfo(%p, %hu, %hu, %p)", ( void * ) pArea, uiIndex, uiType, ( void * ) pItem ) );

   if( uiIndex > pArea->uiFieldCount )
      return ZH_FAILURE;

   pField = pArea->lpFields + uiIndex - 1;
   switch( uiType )
   {
      case DBS_NAME:
         zh_itemPutC( pItem, zh_dynsymName( ( PZH_DYNS ) pField->sym ) );
         break;

      case DBS_TYPE:
      {
         ZH_USHORT uiFlags = 0;
         char szType[ 8 ];
         char cType;
         int iLen = 0;

         switch( pField->uiType )
         {
            case ZH_FT_STRING:
               cType = 'C';
               uiFlags = ZH_FF_NULLABLE | ZH_FF_BINARY | ZH_FF_UNICODE |
                         ZH_FF_ENCRYPTED | ZH_FF_COMPRESSED;
               break;

            case ZH_FT_LOGICAL:
               cType = 'L';
               uiFlags = ZH_FF_NULLABLE;
               break;

            case ZH_FT_DATE:
               cType = 'D';
               uiFlags = ZH_FF_NULLABLE;
               break;

            case ZH_FT_LONG:
               cType = 'N';
               uiFlags = ZH_FF_NULLABLE | ZH_FF_AUTOINC;
               break;

            case ZH_FT_INTEGER:
               cType = 'I';
               uiFlags = ZH_FF_NULLABLE | ZH_FF_AUTOINC;
               break;

            case ZH_FT_DOUBLE:
               cType = 'B';
               uiFlags = ZH_FF_NULLABLE | ZH_FF_AUTOINC;
               break;

            case ZH_FT_FLOAT:
               cType = 'F';
               uiFlags = ZH_FF_NULLABLE | ZH_FF_AUTOINC;
               break;

            case ZH_FT_TIME:
               cType = 'T';
               uiFlags = ZH_FF_NULLABLE;
               break;

            case ZH_FT_TIMESTAMP:
               cType = '@';
               uiFlags = ZH_FF_NULLABLE;
               break;

            case ZH_FT_MODTIME:
               cType = '=';
               break;

            case ZH_FT_ROWVER:
               cType = '^';
               break;

            case ZH_FT_AUTOINC:
               cType = '+';
               uiFlags = ZH_FF_AUTOINC;
               break;

            case ZH_FT_CURRENCY:
               cType = 'Y';
               uiFlags = ZH_FF_NULLABLE;
               break;

            case ZH_FT_CURDOUBLE:
               cType = 'Z';
               uiFlags = ZH_FF_NULLABLE;
               break;

            case ZH_FT_VARLENGTH:
               cType = 'Q';
               uiFlags = ZH_FF_NULLABLE | ZH_FF_BINARY | ZH_FF_UNICODE |
                         ZH_FF_ENCRYPTED | ZH_FF_COMPRESSED;
               break;

            case ZH_FT_ANY:
               cType = 'V';
               uiFlags = ZH_FF_NULLABLE | ZH_FF_BINARY | ZH_FF_UNICODE |
                         ZH_FF_ENCRYPTED | ZH_FF_COMPRESSED;
               break;

            case ZH_FT_MEMO:
               cType = 'M';
               uiFlags = ZH_FF_NULLABLE | ZH_FF_BINARY | ZH_FF_UNICODE |
                         ZH_FF_ENCRYPTED | ZH_FF_COMPRESSED;
               break;

            case ZH_FT_IMAGE:
               cType = 'P';
               break;

            case ZH_FT_BLOB:
               cType = 'W';
               break;

            case ZH_FT_OLE:
               cType = 'G';
               break;

            default:
               cType = 'U';
               break;
         }
         szType[ iLen++ ] = cType;
         uiFlags &= pField->uiFlags;
         if( uiFlags != 0 )
         {
            szType[ iLen++ ] = ':';
            if( uiFlags & ZH_FF_NULLABLE )
               szType[ iLen++ ] = 'N';
            if( uiFlags & ZH_FF_BINARY )
               szType[ iLen++ ] = 'B';
            if( uiFlags & ZH_FF_AUTOINC )
               szType[ iLen++ ] = '+';
            if( uiFlags & ZH_FF_COMPRESSED )
               szType[ iLen++ ] = 'Z';
            if( uiFlags & ZH_FF_ENCRYPTED )
               szType[ iLen++ ] = 'E';
            if( uiFlags & ZH_FF_UNICODE )
               szType[ iLen++ ] = 'U';

         }
         zh_itemPutCL( pItem, szType, iLen );
         break;
      }
      case DBS_LEN:
         zh_itemPutNL( pItem, pField->uiLen );
         break;

      case DBS_DEC:
         zh_itemPutNL( pItem, pField->uiDec );
         break;


      default:
         return ZH_FAILURE;

   }
   return ZH_SUCCESS;
}

/*
 * Determine the name associated with a field number.
 */
static ZH_ERRCODE zh_waFieldName( AREAP pArea, ZH_USHORT uiIndex, char * szName )
{
   LPFIELD pField;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_waFieldName(%p, %hu, %p)", ( void * ) pArea, uiIndex, ( void * ) szName ) );

   if( uiIndex > pArea->uiFieldExtent )
      return ZH_FAILURE;

   pField = pArea->lpFields + uiIndex - 1;
   zh_strncpy( szName, zh_dynsymName( ( PZH_DYNS ) pField->sym ),
               pArea->uiMaxFieldNameLength );
   return ZH_SUCCESS;
}

/*
 * Establish the extent of the array of fields for a WorkArea.
 */
static ZH_ERRCODE zh_waSetFieldExtent( AREAP pArea, ZH_USHORT uiFieldExtent )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_waSetFieldExtent(%p, %hu)", ( void * ) pArea, uiFieldExtent ) );

   pArea->uiFieldExtent = uiFieldExtent;

   /* Alloc field array */
   if( uiFieldExtent )
      pArea->lpFields = ( LPFIELD ) zh_xgrabz( uiFieldExtent * sizeof( FIELD ) );

   return ZH_SUCCESS;
}

/*
 * Obtain the alias of the WorkArea.
 */
static ZH_ERRCODE zh_waAlias( AREAP pArea, char * szAlias )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_waAlias(%p, %p)", ( void * ) pArea, ( void * ) szAlias ) );

   zh_strncpy( szAlias,
      pArea->atomAlias && zh_dynsymAreaHandle( ( PZH_DYNS ) pArea->atomAlias )
      ? zh_dynsymName( ( PZH_DYNS ) pArea->atomAlias ) : "",
      ZH_RDD_MAX_ALIAS_LEN );

   return ZH_SUCCESS;
}

/*
 * Close the table in the WorkArea.
 */
static ZH_ERRCODE zh_waClose( AREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_waClose(%p)", ( void * ) pArea ) );

   /* Clear items */
   SELF_CLEARFILTER( pArea );
   SELF_CLEARREL( pArea );
   SELF_CLEARLOCATE( pArea );

   /* Clear relations that has this area as a child */
   zh_rddCloseAllParentRelations( pArea );

   if( pArea->atomAlias )
      zh_dynsymSetAreaHandle( ( PZH_DYNS ) pArea->atomAlias, 0 );

   return ZH_SUCCESS;
}

/*
 * Retrieve information about the current driver.
 */
static ZH_ERRCODE zh_waInfo( AREAP pArea, ZH_USHORT uiIndex, PZH_ITEM pItem )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_waInfo(%p, %hu, %p)", ( void * ) pArea, uiIndex, ( void * ) pItem ) );

   switch( uiIndex )
   {
      case DBI_ISDBF:
      case DBI_CANPUTREC:
      case DBI_ISFLOCK:
      case DBI_SHARED:
      case DBI_TRANSREC:
         zh_itemPutL( pItem, ZH_FALSE );
         break;

      /*
       * IMHO better to return ZH_FAILURE to notice that it's not supported
       */
      case DBI_GETDELIMITER:
      case DBI_SETDELIMITER:
      case DBI_SEPARATOR:
         zh_itemPutC( pItem, NULL );
         return ZH_FAILURE;

      case DBI_GETHEADERSIZE:
      case DBI_GETRECSIZE:
      case DBI_LOCKCOUNT:
         zh_itemPutNI( pItem, 0 );
         break;

      case DBI_LASTUPDATE:
         zh_itemPutDL( pItem, 0 );
         break;

      case DBI_GETLOCKARRAY:
         zh_arrayNew( pItem, 0 );
         break;

      case DBI_CHILDCOUNT:
      {
         LPDBRELINFO lpdbRelations = pArea->lpdbRelations;
         ZH_USHORT uiCount = 0;
         while( lpdbRelations )
         {
            uiCount++;
            lpdbRelations = lpdbRelations->lpdbriNext;
         }
         zh_itemPutNI( pItem, uiCount );
         break;
      }

      case DBI_BOF:
         zh_itemPutL( pItem, pArea->fBof );
         break;

      case DBI_EOF:
         zh_itemPutL( pItem, pArea->fEof );
         break;

      case DBI_DBFILTER:
         if( pArea->dbfi.abFilterText )
            zh_itemCopy( pItem, pArea->dbfi.abFilterText );
         else
            zh_itemPutC( pItem, NULL );
         break;

      case DBI_FOUND:
         zh_itemPutL( pItem, pArea->fFound );
         break;

      case DBI_FCOUNT:
         zh_itemPutNI( pItem, pArea->uiFieldCount );
         break;

      case DBI_ALIAS:
      {
         char szAlias[ ZH_RDD_MAX_ALIAS_LEN + 1 ];
         if( SELF_ALIAS( pArea, szAlias ) != ZH_SUCCESS )
            return ZH_FAILURE;
         zh_itemPutC( pItem, szAlias );
         break;
      }

      case DBI_TABLEEXT:
      {
         LPRDDNODE pNode = SELF_RDDNODE( pArea );
         zh_itemClear( pItem );
         return pNode ? SELF_RDDINFO( pNode, RDDI_TABLEEXT, 0, pItem ) : ZH_FAILURE;
      }
      case DBI_SCOPEDRELATION:
      {
         int iRelNo = zh_itemGetNI( pItem );
         ZH_BOOL fScoped = ZH_FALSE;

         if( iRelNo > 0 )
         {
            LPDBRELINFO lpdbRelations = pArea->lpdbRelations;
            while( lpdbRelations )
            {
               if( --iRelNo == 0 )
               {
                  fScoped = lpdbRelations->isScoped;
                  break;
               }
               lpdbRelations = lpdbRelations->lpdbriNext;
            }
         }
         zh_itemPutL( pItem, fScoped );
         break;
      }
      case DBI_POSITIONED:
      {
         ZH_ULONG ulRecCount, ulRecNo;
         if( SELF_RECNO( pArea, &ulRecNo ) != ZH_SUCCESS )
            return ZH_FAILURE;
         if( ulRecNo == 0 )
            zh_itemPutL( pItem, ZH_FALSE );
         else if( SELF_RECCOUNT( pArea, &ulRecCount ) != ZH_SUCCESS )
            return ZH_FAILURE;
         else
            zh_itemPutL( pItem, ulRecNo != ulRecCount + 1 );
         break;
      }
      case DBI_CODEPAGE:
         zh_itemPutC( pItem, pArea->cdPage ? pArea->cdPage->id : NULL );
         break;

      case DBI_RM_SUPPORTED:
         zh_itemPutL( pItem, ZH_FALSE );
         break;

      case DBI_DB_VERSION:
         zh_itemPutC( pItem, NULL );
         break;

      case DBI_RDD_VERSION:
         zh_itemPutC( pItem, NULL );
         break;

      default:
         return ZH_FAILURE;
   }
   return ZH_SUCCESS;
}

/*
 * Retrieve information about the current order that SELF could not.
 * Called by SELF_ORDINFO() if uiIndex is not supported.
 */
static ZH_ERRCODE zh_waOrderInfo( AREAP pArea, ZH_USHORT uiIndex, LPDBORDERINFO pInfo )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_waOrderInfo(%p, %hu, %p)", ( void * ) pArea, uiIndex, ( void * ) pInfo ) );

   ZH_SYMBOL_UNUSED( pArea );
   ZH_SYMBOL_UNUSED( uiIndex );

   if( pInfo->itmResult )
      zh_itemClear( pInfo->itmResult );

   /* CA-Cl*pper does not generate RT error when default ORDERINFO() method
    * is called
    */
   #if 0
   zh_errRT_DBCMD( EG_ARG, EDBCMD_BADPARAMETER, NULL, ZH_ERR_FUNCNAME );
   #endif

   return ZH_FAILURE;
}

/*
 * Clear the WorkArea for use.
 */
static ZH_ERRCODE zh_waNewArea( AREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_waNewArea(%p)", ( void * ) pArea ) );

   pArea->valResult = zh_itemNew( NULL );
   pArea->lpdbRelations = NULL;
   pArea->uiParents = 0;
   pArea->uiMaxFieldNameLength = ZH_SYMBOL_NAME_LEN;

   return ZH_SUCCESS;
}

/*
 * Open a data store in the WorkArea.
 * Like in Clipper it's also mapped as Create() method at WA level
 */
static ZH_ERRCODE zh_waOpen( AREAP pArea, LPDBOPENINFO pInfo )
{
   if( ! pArea->atomAlias && pInfo->atomAlias && pInfo->atomAlias[ 0 ] )
   {
      pArea->atomAlias = zh_rddAllocWorkAreaAlias( pInfo->atomAlias,
                                                   ( int ) pInfo->uiArea );
      if( ! pArea->atomAlias )
      {
         SELF_CLOSE( pArea );
         return ZH_FAILURE;
      }
   }
   return ZH_SUCCESS;
}

static ZH_ERRCODE zh_waOrderCondition( AREAP pArea, LPDBORDERCONDINFO param )
{
   if( pArea->lpdbOrdCondInfo )
   {
      if( pArea->lpdbOrdCondInfo->abFor )
         zh_xfree( pArea->lpdbOrdCondInfo->abFor );
      if( pArea->lpdbOrdCondInfo->abWhile )
         zh_xfree( pArea->lpdbOrdCondInfo->abWhile );
      if( pArea->lpdbOrdCondInfo->itmCobFor )
      {
         zh_itemRelease( pArea->lpdbOrdCondInfo->itmCobFor );
      }
      if( pArea->lpdbOrdCondInfo->itmCobWhile )
      {
         zh_itemRelease( pArea->lpdbOrdCondInfo->itmCobWhile );
      }
      if( pArea->lpdbOrdCondInfo->itmCobEval )
      {
         zh_itemRelease( pArea->lpdbOrdCondInfo->itmCobEval );
      }
      if( pArea->lpdbOrdCondInfo->itmStartRecID )
      {
         zh_itemRelease( pArea->lpdbOrdCondInfo->itmStartRecID );
      }
      if( pArea->lpdbOrdCondInfo->itmRecID )
      {
         zh_itemRelease( pArea->lpdbOrdCondInfo->itmRecID );
      }
      zh_xfree( pArea->lpdbOrdCondInfo );
   }
   pArea->lpdbOrdCondInfo = param;

   return ZH_SUCCESS;
}

/*
 * Release all references to a WorkArea.
 */
static ZH_ERRCODE zh_waRelease( AREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_waRelease(%p)", ( void * ) pArea ) );

   /* Free all allocated pointers */
   if( pArea->lpFields )
      zh_xfree( pArea->lpFields );
   if( pArea->valResult )
      zh_itemRelease( pArea->valResult );
   if( pArea->lpdbOrdCondInfo )
      /* intentionally direct call not a method */
      zh_waOrderCondition( pArea, NULL );
   zh_xfree( pArea );
   return ZH_SUCCESS;
}

/*
 * Retrieve the size of the WorkArea structure.
 */
static ZH_ERRCODE zh_waStructSize( AREAP pArea, ZH_USHORT * uiSize )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_waStrucSize(%p, %p)", ( void * ) pArea, ( void * ) uiSize ) );
   ZH_SYMBOL_UNUSED( pArea );

   *uiSize = sizeof( AREA );
   return ZH_SUCCESS;
}

/*
 * Obtain the name of replaceable database driver (RDD) subsystem.
 */
static ZH_ERRCODE zh_waSysName( AREAP pArea, char * pBuffer )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_waSysName(%p, %p)", ( void * ) pArea, ( void * ) pBuffer ) );

   zh_strncpy( pBuffer, SELF_RDDNODE( pArea )->szName,
               ZH_RDD_MAX_DRIVERNAME_LEN );

   return ZH_SUCCESS;
}

/*
 * Evaluate code block for each record in WorkArea.
 */
static ZH_ERRCODE zh_waEval( AREAP pArea, LPDBEVALINFO pEvalInfo )
{
   ZH_LONG lNext = 1;
   ZH_BOOL fEof, fFor;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_waEval(%p, %p)", ( void * ) pArea, ( void * ) pEvalInfo ) );

   if( pEvalInfo->dbsci.itmRecID )
   {
      if( SELF_GOTOID( pArea, pEvalInfo->dbsci.itmRecID ) != ZH_SUCCESS )
         return ZH_FAILURE;
   }
   else if( pEvalInfo->dbsci.lNext )
   {
      lNext = zh_itemGetNL( pEvalInfo->dbsci.lNext );
      if( lNext <= 0 )
         return ZH_SUCCESS;
   }
   else if( ! pEvalInfo->dbsci.itmCobWhile &&
            ! zh_itemGetLX( pEvalInfo->dbsci.fRest ) )
   {
      if( SELF_GOTOP( pArea ) != ZH_SUCCESS )
         return ZH_FAILURE;
   }

   /* TODO: use SKIPSCOPE() method and fRest parameter */

   for( ;; )
   {
      if( SELF_EOF( pArea, &fEof ) != ZH_SUCCESS )
         return ZH_FAILURE;

      if( fEof )
         break;

      if( pEvalInfo->dbsci.itmCobWhile )
      {
         if( SELF_EVALBLOCK( pArea, pEvalInfo->dbsci.itmCobWhile ) != ZH_SUCCESS )
            return ZH_FAILURE;
         if( ! zh_itemGetLX( pArea->valResult ) )
            break;
      }

      if( pEvalInfo->dbsci.itmCobFor )
      {
         if( SELF_EVALBLOCK( pArea, pEvalInfo->dbsci.itmCobFor ) != ZH_SUCCESS )
            return ZH_FAILURE;
         fFor = zh_itemGetLX( pArea->valResult );
      }
      else
         fFor = ZH_TRUE;

      if( fFor )
      {
         if( SELF_EVALBLOCK( pArea, pEvalInfo->itmBlock ) != ZH_SUCCESS )
            return ZH_FAILURE;
      }

      if( pEvalInfo->dbsci.itmRecID || ( pEvalInfo->dbsci.lNext && --lNext < 1 ) )
         break;

      if( SELF_SKIP( pArea, 1 ) != ZH_SUCCESS )
         return ZH_FAILURE;
   }

   return ZH_SUCCESS;
}

/*
 * Locate a record which pass given condition
 */
static ZH_ERRCODE zh_waLocate( AREAP pArea, ZH_BOOL fContinue )
{
   ZH_LONG lNext = 1;
   ZH_BOOL fEof;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_waLocate(%p, %d)", ( void * ) pArea, fContinue ) );

   if( fContinue )
   {
      if( ! pArea->dbsi.itmCobFor )
         return ZH_SUCCESS;

      if( SELF_SKIP( pArea, 1 ) != ZH_SUCCESS )
         return ZH_FAILURE;
   }
   else if( pArea->dbsi.itmRecID )
   {
      if( SELF_GOTOID( pArea, pArea->dbsi.itmRecID ) != ZH_SUCCESS )
         return ZH_FAILURE;
   }
   else if( pArea->dbsi.lNext )
   {
      lNext = zh_itemGetNL( pArea->dbsi.lNext );
      if( lNext <= 0 )
         return ZH_SUCCESS;
   }
   else if( ! pArea->dbsi.itmCobWhile &&
            ! zh_itemGetLX( pArea->dbsi.fRest ) )
   {
      if( SELF_GOTOP( pArea ) != ZH_SUCCESS )
         return ZH_FAILURE;
   }

   pArea->fFound = ZH_FALSE;

   /* TODO: use SKIPSCOPE() method and fRest parameter */

   for( ;; )
   {
      if( SELF_EOF( pArea, &fEof ) != ZH_SUCCESS )
         return ZH_FAILURE;

      if( fEof )
         break;

      if( ! fContinue && pArea->dbsi.itmCobWhile )
      {
         if( SELF_EVALBLOCK( pArea, pArea->dbsi.itmCobWhile ) != ZH_SUCCESS )
            return ZH_FAILURE;
         if( ! zh_itemGetLX( pArea->valResult ) )
            break;
      }

      if( ! pArea->dbsi.itmCobFor )
      {
         pArea->fFound = ZH_TRUE;
         break;
      }
      else
      {
         if( SELF_EVALBLOCK( pArea, pArea->dbsi.itmCobFor ) != ZH_SUCCESS )
            return ZH_FAILURE;

         if( zh_itemGetLX( pArea->valResult ) )
         {
            pArea->fFound = ZH_TRUE;
            break;
         }
      }

      if( ! fContinue &&
          ( pArea->dbsi.itmRecID || ( pArea->dbsi.lNext && --lNext < 1 ) ) )
         break;

      if( SELF_SKIP( pArea, 1 ) != ZH_SUCCESS )
         return ZH_FAILURE;
   }

   return ZH_SUCCESS;
}

/*
 * Copy one or more records from one WorkArea to another.
 */
static ZH_ERRCODE zh_waTrans( AREAP pArea, LPDBTRANSINFO pTransInfo )
{
   ZH_LONG lNext = 1;
   ZH_BOOL fEof, fFor;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_waTrans(%p, %p)", ( void * ) pArea, ( void * ) pTransInfo ) );

   if( pTransInfo->dbsci.itmRecID )
   {
      if( SELF_GOTOID( pArea, pTransInfo->dbsci.itmRecID ) != ZH_SUCCESS )
         return ZH_FAILURE;
   }
   else if( pTransInfo->dbsci.lNext )
   {
      lNext = zh_itemGetNL( pTransInfo->dbsci.lNext );
      if( lNext <= 0 )
         return ZH_SUCCESS;
   }
   else if( ! pTransInfo->dbsci.itmCobWhile &&
            ! zh_itemGetLX( pTransInfo->dbsci.fRest ) )
   {
      if( SELF_GOTOP( pArea ) != ZH_SUCCESS )
         return ZH_FAILURE;
   }

   /* TODO: use SKIPSCOPE() method and fRest parameter */

   for( ;; )
   {
      if( SELF_EOF( pArea, &fEof ) != ZH_SUCCESS )
         return ZH_FAILURE;

      if( fEof )
         break;

      if( pTransInfo->dbsci.itmCobWhile )
      {
         if( SELF_EVALBLOCK( pArea, pTransInfo->dbsci.itmCobWhile ) != ZH_SUCCESS )
            return ZH_FAILURE;
         if( ! zh_itemGetLX( pArea->valResult ) )
            break;
      }

      if( pTransInfo->dbsci.itmCobFor )
      {
         if( SELF_EVALBLOCK( pArea, pTransInfo->dbsci.itmCobFor ) != ZH_SUCCESS )
            return ZH_FAILURE;
         fFor = zh_itemGetLX( pArea->valResult );
      }
      else
         fFor = ZH_TRUE;

      if( fFor )
      {
         if( SELF_TRANSREC( pArea, pTransInfo ) != ZH_SUCCESS )
            return ZH_FAILURE;
      }

      if( pTransInfo->dbsci.itmRecID || ( pTransInfo->dbsci.lNext && --lNext < 1 ) )
         break;

      if( SELF_SKIP( pArea, 1 ) != ZH_SUCCESS )
         return ZH_FAILURE;
   }

   return ZH_SUCCESS;
}

/*
 * Copy a record to another WorkArea.
 */
static ZH_ERRCODE zh_waTransRec( AREAP pArea, LPDBTRANSINFO pTransInfo )
{
   ZH_BOOL bDeleted;
   ZH_BYTE * pRecord;
   ZH_ERRCODE errCode;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_waTransRec(%p, %p)", ( void * ) pArea, ( void * ) pTransInfo ) );

   if( pTransInfo->uiFlags & DBTF_MATCH && pTransInfo->uiFlags & DBTF_PUTREC )
   {
      /* Record deleted? */
      errCode = SELF_DELETED( pArea, &bDeleted );
      if( errCode != ZH_SUCCESS )
         return errCode;

      errCode = SELF_GETREC( pArea, &pRecord );
      if( errCode != ZH_SUCCESS )
         return errCode;

      /* Append a new record */
      errCode = SELF_APPEND( pTransInfo->lpaDest, ZH_TRUE );
      if( errCode != ZH_SUCCESS )
         return errCode;

      /* Copy record */
      errCode = SELF_PUTREC( pTransInfo->lpaDest, pRecord );
   }
   else
   {
      LPDBTRANSITEM pTransItem;
      PZH_ITEM pItem;
      ZH_USHORT uiCount;

      if( pTransInfo->uiFlags & DBTF_RECALL )
         bDeleted = ZH_FALSE;
      else
      {
         /* Record deleted? */
         errCode = SELF_DELETED( pArea, &bDeleted );
         if( errCode != ZH_SUCCESS )
            return errCode;
      }

      /* Append a new record */
      errCode = SELF_APPEND( pTransInfo->lpaDest, ZH_TRUE );
      if( errCode != ZH_SUCCESS )
         return errCode;

      pItem = zh_itemNew( NULL );
      pTransItem = pTransInfo->lpTransItems;
      for( uiCount = pTransInfo->uiItemCount; uiCount; --uiCount )
      {
         errCode = SELF_GETVALUE( pArea, pTransItem->uiSource, pItem );
         if( errCode != ZH_SUCCESS )
            break;
         errCode = SELF_PUTVALUE( pTransInfo->lpaDest,
                                  pTransItem->uiDest, pItem );
         if( errCode != ZH_SUCCESS )
            break;
         ++pTransItem;
      }
      zh_itemRelease( pItem );
   }

   /* Delete the new record if copy fail */
   if( errCode != ZH_SUCCESS )
      SELF_DELETE( pTransInfo->lpaDest );
   else if( bDeleted )
   {
      /* Record with deleted flag */
      if( pTransInfo->uiFlags & DBTF_RECALL )
         errCode = SELF_RECALL( pTransInfo->lpaDest );
      else
         errCode = SELF_DELETE( pTransInfo->lpaDest );
   }

   return errCode;
}

/*
 * Report end of relation.
 */
static ZH_ERRCODE zh_waChildEnd( AREAP pArea, LPDBRELINFO pRelInfo )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_waChildEnd(%p, %p)", ( void * ) pArea, ( void * ) pRelInfo ) );

   if( pRelInfo->isScoped )
   {
      DBORDERINFO pInfo;
      pInfo.itmOrder = NULL;
      pInfo.atomBagName = NULL;
      pInfo.itmResult = zh_itemNew( NULL );
      pInfo.itmNewVal = NULL;
      SELF_ORDINFO( pArea, DBOI_SCOPETOPCLEAR, &pInfo );
      SELF_ORDINFO( pArea, DBOI_SCOPEBOTTOMCLEAR, &pInfo );
      zh_itemRelease( pInfo.itmResult );
   }

   pArea->uiParents--;
   return ZH_SUCCESS;
}

/*
 * Report initialization of a relation.
 */
static ZH_ERRCODE zh_waChildStart( AREAP pArea, LPDBRELINFO pRelInfo )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_waChildStart(%p, %p)", ( void * ) pArea, ( void * ) pRelInfo ) );
   ZH_SYMBOL_UNUSED( pRelInfo );

   pArea->uiParents++;
   return ZH_SUCCESS;
}

/*
 * Force relational movement in child WorkAreas.
 */
static ZH_ERRCODE zh_waSyncChildren( AREAP pArea )
{

   LPDBRELINFO lpdbRelation;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_waSyncChildren(%p)", ( void * ) pArea ) );

   lpdbRelation = pArea->lpdbRelations;
   while( lpdbRelation )
   {
      if( SELF_CHILDSYNC( lpdbRelation->lpaChild, lpdbRelation ) != ZH_SUCCESS )
         return ZH_FAILURE;
      lpdbRelation = lpdbRelation->lpdbriNext;
   }

   return ZH_SUCCESS;
}

/*
 * Clear all relations in the specified WorkArea.
 */
static ZH_ERRCODE zh_waClearRel( AREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_waClearRel(%p)", ( void * ) pArea ) );

   /* Free all relations */
   if( pArea->lpdbRelations )
   {
      int iCurrArea = zh_rddGetCurrentWorkAreaNumber();

      do
      {
         LPDBRELINFO lpdbRelation = pArea->lpdbRelations;

         zh_rddSelectWorkAreaNumber( lpdbRelation->lpaChild->uiArea );
         SELF_CHILDEND( lpdbRelation->lpaChild, lpdbRelation );
         pArea->lpdbRelations = lpdbRelation->lpdbriNext;

         if( lpdbRelation->itmCobExpr )
         {
            zh_itemRelease( lpdbRelation->itmCobExpr );
         }
         if( lpdbRelation->abKey )
         {
            zh_itemRelease( lpdbRelation->abKey );
         }
         zh_xfree( lpdbRelation );
      }
      while( pArea->lpdbRelations );

      zh_rddSelectWorkAreaNumber( iCurrArea );
   }

   return ZH_SUCCESS;
}

/*
 * Obtain the workarea number of the specified relation.
 */
static ZH_ERRCODE zh_waRelArea( AREAP pArea, ZH_USHORT uiRelNo, ZH_USHORT * pRelArea )
{
   LPDBRELINFO lpdbRelations;
   ZH_USHORT uiIndex = 1;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_waRelArea(%p, %hu, %p)", ( void * ) pArea, uiRelNo, ( void * ) pRelArea ) );

   *pRelArea = 0;
   lpdbRelations = pArea->lpdbRelations;
   while( lpdbRelations )
   {
      if( uiIndex++ == uiRelNo )
      {
         *pRelArea = lpdbRelations->lpaChild->uiArea;
         break;
      }
      lpdbRelations = lpdbRelations->lpdbriNext;
   }
   return *pRelArea ? ZH_SUCCESS : ZH_FAILURE;
}

/*
 * Evaluate a block against the relation in specified WorkArea.
 */
static ZH_ERRCODE zh_waRelEval( AREAP pArea, LPDBRELINFO pRelInfo )
{
   ZH_ERRCODE errCode;
   ZH_BOOL fEof;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_waRelEval(%p, %p)", ( void * ) pArea, ( void * ) pRelInfo ) );

   errCode = SELF_EOF( pRelInfo->lpaParent, &fEof );
   if( errCode == ZH_SUCCESS )
   {
      if( fEof )
         errCode = SELF_GOTO( pArea, 0 );
      else
      {
         errCode = SELF_EVALBLOCK( pRelInfo->lpaParent, pRelInfo->itmCobExpr );
         if( errCode == ZH_SUCCESS )
         {
            PZH_ITEM pResult;
            DBORDERINFO pInfo;

            /*
             *  Check the current order
             */
            pResult = pRelInfo->lpaParent->valResult;
            pRelInfo->lpaParent->valResult = NULL;
            memset( &pInfo, 0, sizeof( pInfo ) );
            pInfo.itmResult = zh_itemPutNI( NULL, 0 );
            errCode = SELF_ORDINFO( pArea, DBOI_NUMBER, &pInfo );

            if( errCode == ZH_SUCCESS )
            {
               int iOrder = zh_itemGetNI( pInfo.itmResult );
               if( iOrder != 0 )
               {
                  if( pRelInfo->isScoped )
                  {
                     pInfo.itmNewVal = pResult;
                     errCode = SELF_ORDINFO( pArea, DBOI_SCOPETOP, &pInfo );
                     if( errCode == ZH_SUCCESS )
                        errCode = SELF_ORDINFO( pArea, DBOI_SCOPEBOTTOM, &pInfo );
                  }
                  if( errCode == ZH_SUCCESS )
                     errCode = SELF_SEEK( pArea, ZH_FALSE, pResult, ZH_FALSE );
               }
               else
               {
                  /*
                   * If current order equals to zero, use GOTOID instead of SEEK
                   * Unfortunately it interacts with buggy .prg code which returns
                   * non numerical values from relation expression and RDD accepts
                   * only numerical record ID. In such case SELF_GOTO() works like
                   * SELF_GOEOF() but SELF_GOTOID() reports error. So for Clipper
                   * compatibility SELF_GOTO() is used here but if RDD can use
                   * non numerical record IDs then this method should be overloaded
                   * to use SELF_GOTOID(), [druzus]
                   */
                  #if 0
                  errCode = SELF_GOTOID( pArea, pResult );
                  #endif
                  errCode = SELF_GOTO( pArea, zh_itemGetNL( pResult ) );
                  if( errCode == ZH_SUCCESS )
                  {
                     errCode = SELF_EOF( pArea, &fEof );
                     if( errCode == ZH_SUCCESS )
                        pArea->fFound = ! fEof;
                  }
               }
            }
            zh_itemRelease( pInfo.itmResult );
            zh_itemRelease( pResult );
         }
      }
   }
   return errCode;
}

/*
 * Obtain the character expression of the specified relation.
 */
static ZH_ERRCODE zh_waRelText( AREAP pArea, ZH_USHORT uiRelNo, PZH_ITEM pExpr )
{
   LPDBRELINFO lpdbRelations;
   ZH_USHORT uiIndex = 1;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_waRelText(%p, %hu, %p)", ( void * ) pArea, uiRelNo, ( void * ) pExpr ) );

   lpdbRelations = pArea->lpdbRelations;

   while( lpdbRelations )
   {
      if( uiIndex++ == uiRelNo )
      {
         zh_itemCopy( pExpr, lpdbRelations->abKey );
         return ZH_SUCCESS;
      }
      lpdbRelations = lpdbRelations->lpdbriNext;
   }

   return ZH_FAILURE;
}

/*
 * Set a relation in the parent file.
 */
static ZH_ERRCODE zh_waSetRel( AREAP pArea, LPDBRELINFO lpdbRelInf )
{
   LPDBRELINFO lpdbRelations;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_waSetRel(%p, %p)", ( void * ) pArea, ( void * ) lpdbRelInf ) );

   lpdbRelations = pArea->lpdbRelations;
   if( ! lpdbRelations )
   {
      pArea->lpdbRelations = ( LPDBRELINFO ) zh_xgrab( sizeof( DBRELINFO ) );
      lpdbRelations = pArea->lpdbRelations;
   }
   else
   {
      while( lpdbRelations->lpdbriNext )
         lpdbRelations = lpdbRelations->lpdbriNext;
      lpdbRelations->lpdbriNext = ( LPDBRELINFO ) zh_xgrab( sizeof( DBRELINFO ) );
      lpdbRelations = lpdbRelations->lpdbriNext;
   }
   lpdbRelations->lpaParent = pArea;
   lpdbRelations->lpaChild = lpdbRelInf->lpaChild;
   lpdbRelations->itmCobExpr = lpdbRelInf->itmCobExpr;
   lpdbRelations->isScoped = lpdbRelInf->isScoped;
   lpdbRelations->isOptimized = lpdbRelInf->isOptimized;
   lpdbRelations->abKey = lpdbRelInf->abKey;
   lpdbRelations->lpdbriNext = lpdbRelInf->lpdbriNext;

   return SELF_CHILDSTART( lpdbRelInf->lpaChild, lpdbRelations );
}

/*
 * Clear the active filter expression.
 */
static ZH_ERRCODE zh_waClearFilter( AREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_waClearFilter(%p)", ( void * ) pArea ) );

   /* Free all items */
   if( pArea->dbfi.itmCobExpr )
   {
      zh_itemRelease( pArea->dbfi.itmCobExpr );
      pArea->dbfi.itmCobExpr = NULL;
   }
   if( pArea->dbfi.abFilterText )
   {
      zh_itemRelease( pArea->dbfi.abFilterText );
      pArea->dbfi.abFilterText = NULL;
   }
   pArea->dbfi.fOptimized = ZH_FALSE;
   pArea->dbfi.fFilter = ZH_FALSE;

   return ZH_SUCCESS;
}

/*
 * Clear the active locate expression.
 */
static ZH_ERRCODE zh_waClearLocate( AREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_waClearLocate(%p)", ( void * ) pArea ) );

   /* Free all items */
   if( pArea->dbsi.itmCobFor )
   {
      zh_itemRelease( pArea->dbsi.itmCobFor );
      pArea->dbsi.itmCobFor = NULL;
   }
   if( pArea->dbsi.lpstrFor )
   {
      zh_itemRelease( pArea->dbsi.lpstrFor );
      pArea->dbsi.lpstrFor = NULL;
   }
   if( pArea->dbsi.itmCobWhile )
   {
      zh_itemRelease( pArea->dbsi.itmCobWhile );
      pArea->dbsi.itmCobWhile = NULL;
   }
   if( pArea->dbsi.lpstrWhile )
   {
      zh_itemRelease( pArea->dbsi.lpstrWhile );
      pArea->dbsi.lpstrWhile = NULL;
   }
   if( pArea->dbsi.lNext )
   {
      zh_itemRelease( pArea->dbsi.lNext );
      pArea->dbsi.lNext = NULL;
   }
   if( pArea->dbsi.itmRecID )
   {
      zh_itemRelease( pArea->dbsi.itmRecID );
      pArea->dbsi.itmRecID = NULL;
   }
   if( pArea->dbsi.fRest )
   {
      zh_itemRelease( pArea->dbsi.fRest );
      pArea->dbsi.fRest = NULL;
   }

   return ZH_SUCCESS;
}

/*
 * Return filter condition of the specified WorkArea.
 */
static ZH_ERRCODE zh_waFilterText( AREAP pArea, PZH_ITEM pFilter )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_waFilterText(%p, %p)", ( void * ) pArea, ( void * ) pFilter ) );

   if( pArea->dbfi.abFilterText )
      zh_itemCopy( pFilter, pArea->dbfi.abFilterText );

   return ZH_SUCCESS;
}

/*
 * Set the filter condition for the specified WorkArea.
 */
static ZH_ERRCODE zh_waSetFilter( AREAP pArea, LPDBFILTERINFO pFilterInfo )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_waSetFilter(%p, %p)", ( void * ) pArea, ( void * ) pFilterInfo ) );

   /* Clear the active filter expression */
   if( SELF_CLEARFILTER( pArea ) != ZH_SUCCESS )
      return ZH_FAILURE;

   if( pFilterInfo->itmCobExpr )
   {
      pArea->dbfi.itmCobExpr = zh_itemNew( pFilterInfo->itmCobExpr );
   }
   if( pFilterInfo->abFilterText )
   {
      pArea->dbfi.abFilterText = zh_itemNew( pFilterInfo->abFilterText );
   }
   pArea->dbfi.fOptimized = pFilterInfo->fOptimized;
   pArea->dbfi.fFilter = ZH_TRUE;

   return ZH_SUCCESS;
}

/*
 * Set the locate scope for the specified WorkArea.
 */
static ZH_ERRCODE zh_waSetLocate( AREAP pArea, LPDBSCOPEINFO pScopeInfo )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_waSetLocate(%p, %p)", ( void * ) pArea, ( void * ) pScopeInfo ) );

   /* Clear the active locate expression */
   if( SELF_CLEARLOCATE( pArea ) != ZH_SUCCESS )
      return ZH_FAILURE;

   if( pScopeInfo->itmCobFor )
      pArea->dbsi.itmCobFor = zh_itemNew( pScopeInfo->itmCobFor );

   if( pScopeInfo->lpstrFor )
      pArea->dbsi.lpstrFor = zh_itemNew( pScopeInfo->lpstrFor );

   if( pScopeInfo->itmCobWhile )
      pArea->dbsi.itmCobWhile = zh_itemNew( pScopeInfo->itmCobWhile );

   if( pScopeInfo->lpstrWhile )
      pArea->dbsi.lpstrWhile = zh_itemNew( pScopeInfo->lpstrWhile );

   if( pScopeInfo->lNext )
      pArea->dbsi.lNext = zh_itemNew( pScopeInfo->lNext );

   if( pScopeInfo->itmRecID )
      pArea->dbsi.itmRecID = zh_itemNew( pScopeInfo->itmRecID );

   if( pScopeInfo->fRest )
      pArea->dbsi.fRest = zh_itemNew( pScopeInfo->fRest );

   pArea->dbsi.fIgnoreFilter     = pScopeInfo->fIgnoreFilter;
   pArea->dbsi.fIncludeDeleted   = pScopeInfo->fIncludeDeleted;
   pArea->dbsi.fLast             = pScopeInfo->fLast;
   pArea->dbsi.fIgnoreDuplicates = pScopeInfo->fIgnoreDuplicates;
   pArea->dbsi.fBackward         = pScopeInfo->fBackward;
   pArea->dbsi.fOptimized        = pScopeInfo->fOptimized;

   return ZH_SUCCESS;
}

/*
 * Compile a character expression.
 */
static ZH_ERRCODE zh_waCompile( AREAP pArea, const char * pExpr )
{
   PZH_MACRO pMacro;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_waCompile(%p, %p)", ( void * ) pArea, ( const void * ) pExpr ) );

   pMacro = zh_macroCompile( pExpr );
   if( pMacro )
   {
      pArea->valResult = zh_itemPutPtr( pArea->valResult, ( void * ) pMacro );
      return ZH_SUCCESS;
   }
   else
      return ZH_FAILURE;
}

/*
 * Raise a runtime error.
 */
static ZH_ERRCODE zh_waError( AREAP pArea, PZH_ITEM pError )
{
   char szRddName[ ZH_RDD_MAX_DRIVERNAME_LEN + 1 ];

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_waError(%p, %p)", ( void * ) pArea, ( void * ) pError ) );

   if( pArea && pArea->lprfsHost->sysName )
      SELF_SYSNAME( pArea, szRddName );
   else
      zh_strncpy( szRddName, "???DRIVER", ZH_RDD_MAX_DRIVERNAME_LEN );
   zh_errPutSeverity( pError, ES_ERROR );
   zh_errPutSubSystem( pError, szRddName );
   return zh_errLaunch( pError );
}

/*
 * Evaluate a code block.
 */
static ZH_ERRCODE zh_waEvalBlock( AREAP pArea, PZH_ITEM pBlock )
{
   PZH_ITEM pItem;
   int iCurrArea, iUsedArea;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_waEvalBlock(%p, %p)", ( void * ) pArea, ( void * ) pBlock ) );

   iCurrArea = zh_rddGetCurrentWorkAreaNumber();
   iUsedArea = pArea->uiArea;
   if( iCurrArea != iUsedArea )
      zh_rddSelectWorkAreaNumber( iUsedArea );

   pItem = zh_vmEvalBlockOrMacro( pBlock );

   if( ( AREAP ) zh_rddGetWorkAreaPointer( iUsedArea ) != pArea )
      return ZH_FAILURE;

   if( ! pArea->valResult )
      pArea->valResult = zh_itemNew( NULL );
   zh_itemMove( pArea->valResult, pItem );

   zh_rddSelectWorkAreaNumber( iCurrArea );

   return zh_vmRequestQuery() ? ZH_FAILURE : ZH_SUCCESS;
}

/*
 * RDD info
 */
static ZH_ERRCODE zh_waRddInfo( LPRDDNODE pRDD, ZH_USHORT uiIndex, ZH_ULONG ulConnection, PZH_ITEM pItem )
{
   ZH_BOOL fResult;
   int iResult;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_rddInfo(%p, %hu, %lu, %p)", ( void * ) pRDD, uiIndex, ulConnection, ( void * ) pItem ) );

   ZH_SYMBOL_UNUSED( pRDD );
   ZH_SYMBOL_UNUSED( ulConnection );

   switch( uiIndex )
   {
      case RDDI_ISDBF:
      case RDDI_CANPUTREC:
      case RDDI_LOCAL:
      case RDDI_REMOTE:
      case RDDI_RECORDMAP:
      case RDDI_ENCRYPTION:
      case RDDI_AUTOLOCK:
      case RDDI_STRUCTORD:
      case RDDI_LARGEFILE:
      case RDDI_MULTITAG:
      case RDDI_SORTRECNO:
      case RDDI_MULTIKEY:
      case RDDI_BLOB_SUPPORT:
         zh_itemPutL( pItem, ZH_FALSE );
         break;

      case RDDI_CONNECTION:
      case RDDI_TABLETYPE:
      case RDDI_MEMOTYPE:
      case RDDI_MEMOVERSION:
         zh_itemPutNI( pItem, 0 );
         break;

      case RDDI_STRICTREAD:
         fResult = zh_setGetStrictRead();
         if( zh_itemType( pItem ) & ZH_IT_LOGICAL )
            zh_setSetItem( ZH_SET_STRICTREAD, pItem );
         zh_itemPutL( pItem, fResult );
         break;
      case RDDI_OPTIMIZE:
         fResult = zh_setGetOptimize();
         if( zh_itemType( pItem ) & ZH_IT_LOGICAL )
            zh_setSetItem( ZH_SET_OPTIMIZE, pItem );
         zh_itemPutL( pItem, fResult );
         break;
      case RDDI_FORCEOPT:
         fResult = zh_setGetForceOpt();
         if( zh_itemType( pItem ) & ZH_IT_LOGICAL )
            zh_setSetItem( ZH_SET_FORCEOPT, pItem );
         zh_itemPutL( pItem, fResult );
         break;
      case RDDI_AUTOOPEN:
         fResult = zh_setGetAutOpen();
         if( zh_itemType( pItem ) & ZH_IT_LOGICAL )
            zh_setSetItem( ZH_SET_AUTOPEN, pItem );
         zh_itemPutL( pItem, fResult );
         break;
      case RDDI_AUTOORDER:
         iResult = zh_setGetAutOrder();
         if( zh_itemType( pItem ) & ZH_IT_NUMERIC )
            zh_setSetItem( ZH_SET_AUTORDER, pItem );
         zh_itemPutNI( pItem, iResult );
         break;
      case RDDI_AUTOSHARE:
         fResult = zh_setGetAutoShare();
         if( zh_itemType( pItem ) & ZH_IT_LOGICAL )
            zh_setSetItem( ZH_SET_AUTOSHARE, pItem );
         zh_itemPutL( pItem, fResult );
         break;
      case RDDI_LOCKSCHEME:
         iResult = zh_setGetDBFLockScheme();
         if( zh_itemType( pItem ) & ZH_IT_NUMERIC )
            zh_setSetItem( ZH_SET_DBFLOCKSCHEME, pItem );
         zh_itemPutNI( pItem, iResult );
         break;
      case RDDI_MEMOBLOCKSIZE:
         iResult = zh_setGetMBlockSize();
         if( zh_itemType( pItem ) & ZH_IT_NUMERIC )
            zh_setSetItem( ZH_SET_MBLOCKSIZE, pItem );
         zh_itemPutNI( pItem, iResult );
         break;
      case RDDI_MEMOEXT:
      {
         const char * szExt = zh_setGetMFileExt();
         char * szResult = szExt ? zh_strdup( szExt ) : NULL;
         if( zh_itemType( pItem ) & ZH_IT_STRING )
            zh_setSetItem( ZH_SET_MFILEEXT, pItem );
         zh_itemPutCPtr( pItem, szResult );
         break;
      }
      case RDDI_TABLEEXT:
      case RDDI_ORDBAGEXT:
      case RDDI_ORDEREXT:
      case RDDI_ORDSTRUCTEXT:
      case RDDI_DELIMITER:
      case RDDI_SEPARATOR:
      case RDDI_TRIGGER:
      case RDDI_PENDINGTRIGGER:
         zh_itemPutC( pItem, NULL );
         /* fallthrough */ /* return ZH_FAILURE */

      default:
         return ZH_FAILURE;
   }
   return ZH_SUCCESS;
}

/*
 * Raise a runtime error if an method is not defined.
 */
static ZH_ERRCODE zh_waUnsupported( AREAP pArea )
{
   PZH_ITEM pError;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_waUnsupported(%p)", ( void * ) pArea ) );

   pError = zh_errNew();
   zh_errPutGenCode( pError, EG_UNSUPPORTED );
   zh_errPutDescription( pError, zh_langDGetErrorDesc( EG_UNSUPPORTED ) );
   SELF_ERROR( pArea, pError );
   zh_itemRelease( pError );

   return ZH_FAILURE;
}

/*
 * Raise a runtime error if an method is not defined.
 */
static ZH_ERRCODE zh_waRddUnsupported( LPRDDNODE pRDD )
{
   PZH_ITEM pError;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_waRDDUnsupported(%p)", ( void * ) pRDD ) );

   pError = zh_errNew();
   zh_errPutGenCode( pError, EG_UNSUPPORTED );
   zh_errPutDescription( pError, zh_langDGetErrorDesc( EG_UNSUPPORTED ) );

   zh_errPutSeverity( pError, ES_ERROR );
   zh_errPutSubSystem( pError, pRDD->szName );
   zh_errLaunch( pError );
   zh_itemRelease( pError );

   return ZH_FAILURE;
}

#if 0
/*
 * Empty method.
 */
static ZH_ERRCODE zh_waNull( AREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_waNull(%p)", ( void * ) pArea ) );

   ZH_SYMBOL_UNUSED( pArea );

   return ZH_SUCCESS;
}
#endif

/*
 * The default virtual method table for all WorkAreas.
 */
static const RDDFUNCS waTable =
{
   /* Movement and positioning methods */
/* ( DBENTRYP_BP )   */ zh_waBof,               /* Bof        */
/* ( DBENTRYP_BP )   */ zh_waEof,               /* Eof        */
/* ( DBENTRYP_BP )   */ zh_waFound,             /* Found      */
   ( DBENTRYP_V )       zh_waUnsupported,       /* GoBottom   */
   ( DBENTRYP_UL )      zh_waUnsupported,       /* GoTo       */
   ( DBENTRYP_I )       zh_waUnsupported,       /* GoToId     */
   ( DBENTRYP_V )       zh_waUnsupported,       /* GoTop      */
   ( DBENTRYP_BIB )     zh_waUnsupported,       /* Seek       */
/* ( DBENTRYP_L )    */ zh_waSkip,              /* Skip       */
/* ( DBENTRYP_L )    */ zh_waSkipFilter,        /* SkipFilter */
   ( DBENTRYP_L )       zh_waUnsupported,       /* SkipRaw    */

   /* Data management */
/* ( DBENTRYP_VF )   */ zh_waAddField,          /* AddField       */
   ( DBENTRYP_B )       zh_waUnsupported,       /* Append         */
/* ( DBENTRYP_I )    */ zh_waCreateFields,      /* CreateFields   */
   ( DBENTRYP_V )       zh_waUnsupported,       /* DeleteRec      */
   ( DBENTRYP_BP )      zh_waUnsupported,       /* Deleted        */
/* ( DBENTRYP_SP )   */ zh_waFieldCount,        /* FieldCount     */
   ( DBENTRYP_VF )      zh_waUnsupported,       /* FieldDisplay   */
/* ( DBENTRYP_SSI )  */ zh_waFieldInfo,         /* FieldInfo      */
/* ( DBENTRYP_SCP )  */ zh_waFieldName,         /* FieldName      */
   ( DBENTRYP_V )       zh_waUnsupported,       /* Flush          */
   ( DBENTRYP_PP )      zh_waUnsupported,       /* GetRec         */
   ( DBENTRYP_SI )      zh_waUnsupported,       /* GetValue       */
   ( DBENTRYP_SVL )     zh_waUnsupported,       /* GetVarLen      */
   ( DBENTRYP_V )       zh_waUnsupported,       /* GoCold         */
   ( DBENTRYP_V )       zh_waUnsupported,       /* GoHot          */
   ( DBENTRYP_P )       zh_waUnsupported,       /* PutRec         */
   ( DBENTRYP_SI )      zh_waUnsupported,       /* PutValue       */
   ( DBENTRYP_V )       zh_waUnsupported,       /* Recall         */
   ( DBENTRYP_ULP )     zh_waUnsupported,       /* RecCount       */
   ( DBENTRYP_ISI )     zh_waUnsupported,       /* RecInfo        */
   ( DBENTRYP_ULP )     zh_waUnsupported,       /* RecNo          */
   ( DBENTRYP_I )       zh_waUnsupported,       /* RecId          */
/* ( DBENTRYP_S )    */ zh_waSetFieldExtent,    /* SetFieldExtent */

   /* WorkArea/Database management */
/* ( DBENTRYP_CP )   */ zh_waAlias,             /* Alias       */
/* ( DBENTRYP_V )    */ zh_waClose,             /* Close       */
   /* Like in Clipper map CREATE() method at work area level to OPEN() */
/* ( DBENTRYP_VO )   */ zh_waOpen,              /* Create      */
/* ( DBENTRYP_SI )   */ zh_waInfo,              /* Info        */
/* ( DBENTRYP_V )    */ zh_waNewArea,           /* NewArea     */
/* ( DBENTRYP_VO )   */ zh_waOpen,              /* Open        */
/* ( DBENTRYP_V )    */ zh_waRelease,           /* Release     */
/* ( DBENTRYP_SP )   */ zh_waStructSize,        /* StructSize  */
/* ( DBENTRYP_CP )   */ zh_waSysName,           /* SysName     */
/* ( DBENTRYP_VEI )  */ zh_waEval,              /* Eval        */
   ( DBENTRYP_V )       zh_waUnsupported,       /* Pack        */
   ( DBENTRYP_LSP )     zh_waUnsupported,       /* PackRec     */
   ( DBENTRYP_VS )      zh_waUnsupported,       /* Sort        */
/* ( DBENTRYP_VT )   */ zh_waTrans,             /* Trans       */
/* ( DBENTRYP_VT )   */ zh_waTransRec,          /* TransRec    */
   ( DBENTRYP_V )       zh_waUnsupported,       /* Zap         */

   /* Relational Methods */
/* ( DBENTRYP_VR )   */ zh_waChildEnd,          /* ChildEnd      */
/* ( DBENTRYP_VR )   */ zh_waChildStart,        /* ChildStart    */
   ( DBENTRYP_VR )      zh_waUnsupported,       /* ChildSync     */
/* ( DBENTRYP_V )    */ zh_waSyncChildren,      /* SyncChildren  */
/* ( DBENTRYP_V )    */ zh_waClearRel,          /* ClearRel      */
   ( DBENTRYP_V )       zh_waUnsupported,       /* ForceRel      */
/* ( DBENTRYP_SSP )  */ zh_waRelArea,           /* RelArea       */
/* ( DBENTRYP_VR )   */ zh_waRelEval,           /* RelEval       */
/* ( DBENTRYP_SI )   */ zh_waRelText,           /* RelText       */
/* ( DBENTRYP_VR )   */ zh_waSetRel,            /* SetRel        */

   /* Order Management */
   ( DBENTRYP_VOI )     zh_waUnsupported,       /* OrderListAdd      */
   ( DBENTRYP_V )       zh_waUnsupported,       /* OrderListClear    */
   ( DBENTRYP_VOI )     zh_waUnsupported,       /* OrderListDelete   */
   ( DBENTRYP_VOI )     zh_waUnsupported,       /* OrderListFocus    */
   ( DBENTRYP_V )       zh_waUnsupported,       /* OrderListRebuild  */
/* ( DBENTRYP_VOO )  */ zh_waOrderCondition,    /* OrderCondition    */
   ( DBENTRYP_VOC )     zh_waUnsupported,       /* OrderCreate       */
   ( DBENTRYP_VOI )     zh_waUnsupported,       /* OrderDestroy      */
/* ( DBENTRYP_SVOI ) */ zh_waOrderInfo,         /* OrderInfo         */

   /* Filters and Scope Settings */
/* ( DBENTRYP_V )    */ zh_waClearFilter,       /* ClearFilter  */
/* ( DBENTRYP_V )    */ zh_waClearLocate,       /* ClearLocate  */
   ( DBENTRYP_V )       zh_waUnsupported,       /* ClearScope   */
   ( DBENTRYP_VPLP )    zh_waUnsupported,       /* CountScope   */
/* ( DBENTRYP_I )    */ zh_waFilterText,        /* FilterText   */
   ( DBENTRYP_SI )      zh_waUnsupported,       /* ScopeInfo    */
/* ( DBENTRYP_VFI )  */ zh_waSetFilter,         /* SetFilter    */
/* ( DBENTRYP_VLO )  */ zh_waSetLocate,         /* SetLocate    */
   ( DBENTRYP_VOS )     zh_waUnsupported,       /* SetScope     */
   ( DBENTRYP_VPL )     zh_waUnsupported,       /* SkipScope    */
/* ( DBENTRYP_B )    */ zh_waLocate,            /* Locate       */

   /* Miscellaneous */
/* ( DBENTRYP_CC )   */ zh_waCompile,           /* Compile    */
/* ( DBENTRYP_I )    */ zh_waError,             /* Error      */
/* ( DBENTRYP_I )    */ zh_waEvalBlock,         /* EvalBlock  */

   /* Network operations */
   ( DBENTRYP_VSP )     zh_waUnsupported,       /* RawLock  */
   ( DBENTRYP_VL )      zh_waUnsupported,       /* Lock     */
   ( DBENTRYP_I )       zh_waUnsupported,       /* UnLock   */

   /* Memofile functions */
   ( DBENTRYP_V )       zh_waUnsupported,       /* CloseMemFile   */
   ( DBENTRYP_VO )      zh_waUnsupported,       /* CreateMemFile  */
   ( DBENTRYP_SCCS )    zh_waUnsupported,       /* GetValueFile   */
   ( DBENTRYP_VO )      zh_waUnsupported,       /* OpenMemFile    */
   ( DBENTRYP_SCCS )    zh_waUnsupported,       /* PutValueFile   */

   /* Database file header handling */
   ( DBENTRYP_V )       zh_waUnsupported,       /* ReadDBHeader   */
   ( DBENTRYP_V )       zh_waUnsupported,       /* WriteDBHeader  */

   /* non WorkArea functions */
   ( DBENTRYP_R )       NULL,                   /* Init    */
   ( DBENTRYP_R )       NULL,                   /* Exit    */
   ( DBENTRYP_RVVL )    zh_waRddUnsupported,    /* Drop    */
   ( DBENTRYP_RVVL )    zh_waRddUnsupported,    /* Exists  */
   ( DBENTRYP_RVVVL )   zh_waRddUnsupported,    /* Rename  */
/* ( DBENTRYP_RSLV ) */ zh_waRddInfo,           /* RddInfo */

   /* Special and reserved methods */
   ( DBENTRYP_SVP )   zh_waUnsupported          /* WhoCares */
};

#define ZH_RDD_POOL_ALLOCSIZE  128
/* common for all threads list of registered RDDs */
static ZH_CRITICAL_NEW( s_rddMtx );
static LPRDDNODE * s_RddList    = NULL;   /* Registered RDDs pool */
static ZH_USHORT   s_uiRddMax   = 0;      /* Size of RDD pool */
static ZH_USHORT   s_uiRddCount = 0;      /* Number of registered RDD */

static ZH_RDDACCEPT * s_rddRedirAccept  = NULL;
static ZH_USHORT      s_uiRddRedirMax   = 0;
static ZH_USHORT      s_uiRddRedirCount = 0;

/*
 * Get RDD node pointer
 */
LPRDDNODE zh_rddGetNode( ZH_USHORT uiNode )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_rddGetNode(%hu)", uiNode ) );

   return uiNode < s_uiRddCount ? s_RddList[ uiNode ] : NULL;
}

PZH_ITEM zh_rddList( ZH_USHORT uiType )
{
   ZH_USHORT uiCount, uiIndex, uiRdds;
   PZH_ITEM pRddArray;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_rddList(%hu)", uiType ) );

   for( uiCount = uiRdds = 0; uiCount < s_uiRddCount; ++uiCount )
   {
      if( uiType == 0 || s_RddList[ uiCount ]->uiType == uiType )
         ++uiRdds;
   }
   pRddArray = zh_itemArrayNew( uiRdds );
   for( uiCount = uiIndex = 0; uiCount < s_uiRddCount && uiIndex < uiRdds; ++uiCount )
   {
      LPRDDNODE pNode = s_RddList[ uiCount ];
      if( uiType == 0 || pNode->uiType == uiType )
         zh_arraySetC( pRddArray, ++uiIndex, pNode->szName );
   }
   return pRddArray;
}

/*
 * Find a RDD node.
 */
LPRDDNODE zh_rddFindNode( const char * szDriver, ZH_USHORT * uiIndex )
{
   ZH_USHORT uiCount;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_rddFindNode(%s, %p)", szDriver, ( void * ) uiIndex ) );

   for( uiCount = 0; uiCount < s_uiRddCount; uiCount++ )
   {
      LPRDDNODE pNode = s_RddList[ uiCount ];
      if( strcmp( pNode->szName, szDriver ) == 0 ) /* Matched RDD */
      {
         if( uiIndex )
            *uiIndex = uiCount;
         return pNode;
      }
   }
   if( uiIndex )
      *uiIndex = 0;
   return NULL;
}

/*
 * Find a RDD node respecting file/table name
 */
LPRDDNODE zh_rddFindFileNode( LPRDDNODE pRddNode, const char * szFileName )
{
   ZH_USHORT uiCount;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_rddFindFileNode(%p, %s)", ( void * ) pRddNode, szFileName ) );

   if( szFileName && szFileName[ 0 ] && s_uiRddRedirCount )
   {
      for( uiCount = 0; uiCount < s_uiRddRedirCount; uiCount++ )
      {
         LPRDDNODE pNode = s_rddRedirAccept[ uiCount ]( pRddNode, szFileName );
         if( pNode )
            return pNode;
      }
   }

   return pRddNode;
}

/*
 * dummy RDD file/table name redirector
 */
static LPRDDNODE zh_rddDummyFileAccept( LPRDDNODE pRddNode, const char * szFileName )
{
   ZH_SYMBOL_UNUSED( pRddNode );
   ZH_SYMBOL_UNUSED( szFileName );

   return NULL;
}
/*
 * Add new RDD file/table name redirector
 */
void zh_rddSetFileRedirector( ZH_RDDACCEPT funcAccept, ZH_BOOL fEnable )
{
   ZH_USHORT uiCount, uiFree;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_rddSetFileRedirector(%p, %d)", ( void * ) funcAccept, fEnable ) );

   zh_threadEnterCriticalSection( &s_rddMtx );
   uiFree = s_uiRddRedirCount + 1;
   for( uiCount = 0; uiCount < s_uiRddRedirCount; uiCount++ )
   {
      if( s_rddRedirAccept[ uiCount ] == funcAccept )
      {
         if( ! fEnable )
            s_rddRedirAccept[ uiCount ] = zh_rddDummyFileAccept;
         return;
      }
      else if( s_rddRedirAccept[ uiCount ] == zh_rddDummyFileAccept )
         uiFree = uiCount;
   }
   if( uiFree < s_uiRddRedirCount )
      s_rddRedirAccept[ uiFree ] = funcAccept;
   else
   {
      if( s_uiRddRedirCount == s_uiRddRedirMax )
      {
         s_uiRddRedirMax += ZH_RDD_POOL_ALLOCSIZE;
         s_rddRedirAccept = ( ZH_RDDACCEPT * )
            zh_xrealloc( s_rddRedirAccept, sizeof( ZH_RDDACCEPT ) * s_uiRddRedirMax );
      }
      s_rddRedirAccept[ s_uiRddRedirCount ] = funcAccept;
      s_uiRddRedirCount++;
   }
   zh_threadLeaveCriticalSection( &s_rddMtx );
}

/*
 * Shutdown the RDD system.
 */
void zh_rddShutDown( void )
{
   ZH_USHORT uiCount;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_rddShutDown()" ) );

   zh_rddCloseDetachedAreas();

   if( s_uiRddCount > 0 )
   {
      for( uiCount = 0; uiCount < s_uiRddCount; uiCount++ )
      {
         if( s_RddList[ uiCount ]->pTable.exit != NULL )
         {
            SELF_EXIT( s_RddList[ uiCount ] );
         }
         zh_xfree( s_RddList[ uiCount ] );
      }
      zh_xfree( s_RddList );
      s_RddList = NULL;
      s_uiRddMax = s_uiRddCount = 0;
   }
   if( s_uiRddRedirCount )
   {
      zh_xfree( s_rddRedirAccept );
      s_rddRedirAccept = NULL;
      s_uiRddRedirMax = s_uiRddRedirCount = 0;
   }
}

/*
 * Register a RDD driver.
 */
int zh_rddRegister( const char * szDriver, ZH_USHORT uiType )
{
   LPRDDNODE pRddNewNode;
   PZH_DYNS pGetFuncTable;
   char szGetFuncTable[ ZH_RDD_MAX_DRIVERNAME_LEN + 14 ];
   ZH_USHORT uiFunctions = 0;
   int iResult;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_rddRegister(%s, %hu)", szDriver, uiType ) );

   if( zh_rddFindNode( szDriver, NULL ) )    /* Duplicated RDD */
      return 1;

   zh_snprintf( szGetFuncTable, sizeof( szGetFuncTable ), "%s_GETFUNCTABLE",
                szDriver );
   pGetFuncTable = zh_dynsymFindName( szGetFuncTable );
   if( ! pGetFuncTable )
      return 2;              /* Not valid RDD */

   /* Create a new RDD node */
   pRddNewNode = ( LPRDDNODE ) zh_xgrabz( sizeof( RDDNODE ) );

   /* Fill the new RDD node */
   zh_strncpy( pRddNewNode->szName, szDriver, sizeof( pRddNewNode->szName ) - 1 );
   pRddNewNode->uiType = uiType;
   pRddNewNode->rddID = s_uiRddCount;
   pRddNewNode->rddSuperID = ( ZH_USHORT ) ( -1 );

   /* Call <szDriver>_GETFUNCTABLE() */
   zh_vmPushDynSym( pGetFuncTable );
   zh_vmPushNil();
   zh_vmPushPointer( ( void * ) &uiFunctions );
   zh_vmPushPointer( ( void * ) &pRddNewNode->pTable );
   zh_vmPushPointer( ( void * ) &pRddNewNode->pSuperTable );
   zh_vmPushInteger( s_uiRddCount );
   zh_vmPushPointer( ( void * ) &pRddNewNode->rddSuperID );
   zh_vmProc( 5 );
   if( zh_parnidef( -1, ZH_FAILURE ) != ZH_SUCCESS )
      iResult = 3;                        /* Invalid FUNCTABLE */
   else
   {
      zh_threadEnterCriticalSection( &s_rddMtx );
      /* repeat the test to protect against possible registering RDD by
       *  <szDriver>_GETFUNCTABLE()
       */
      if( ! zh_rddFindNode( szDriver, NULL ) )    /* Duplicated RDD */
      {
         if( s_uiRddCount == s_uiRddMax )
         {
            s_uiRddMax += ZH_RDD_POOL_ALLOCSIZE;
            s_RddList = ( LPRDDNODE * )
                  zh_xrealloc( s_RddList, sizeof( LPRDDNODE ) * s_uiRddMax );
         }
         s_RddList[ s_uiRddCount ] = pRddNewNode;   /* Add the new RDD node */
         s_uiRddCount++;
         iResult = 0;
      }
      else
         iResult = 1;
      zh_threadLeaveCriticalSection( &s_rddMtx );
   }

   if( iResult != 0 )
      zh_xfree( pRddNewNode );
   else if( pRddNewNode->pTable.init != NULL )
      SELF_INIT( pRddNewNode );

   return iResult;
}

/*
 * pTable - a table in new RDDNODE that will be filled
 * pSubTable - a table with a list of supported functions
 * pSuperTable - a current table in a RDDNODE
 * szDrvName - a driver name that will be inherited
 */
ZH_ERRCODE zh_rddInheritEx( RDDFUNCS * pTable, const RDDFUNCS * pSubTable,
                            RDDFUNCS * pSuperTable, const char * szDrvName,
                            ZH_USHORT * puiSuperRddId )
{
   LPRDDNODE pRddNode;
   ZH_USHORT uiCount;
   DBENTRYP_V * pFunction;
   const DBENTRYP_V * pSubFunction;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_rddInheritEx(%p, %p, %p, %s, %p)", ( void * ) pTable, ( const void * ) pSubTable, ( void * ) pSuperTable, szDrvName, ( void * ) puiSuperRddId ) );

   if( ! pTable )
   {
      return ZH_FAILURE;
   }

   /* Copy the pSuperTable into pTable */
   if( ! szDrvName || ! *szDrvName )
   {
      /* no name for inherited driver - use the default one */
      memcpy( pTable, &waTable, sizeof( RDDFUNCS ) );
      memcpy( pSuperTable, &waTable, sizeof( RDDFUNCS ) );
      if( puiSuperRddId )
         *puiSuperRddId = ( ZH_USHORT ) -1;
   }
   else
   {
      char szSuperName[ ZH_RDD_MAX_DRIVERNAME_LEN + 1 ];
      zh_strncpyUpper( szSuperName, szDrvName, sizeof( szSuperName ) - 1 );
      pRddNode = zh_rddFindNode( szSuperName, NULL );

      if( ! pRddNode )
         return ZH_FAILURE;

      memcpy( pTable, &pRddNode->pTable, sizeof( RDDFUNCS ) );
      memcpy( pSuperTable, &pRddNode->pTable, sizeof( RDDFUNCS ) );
      if( puiSuperRddId )
         *puiSuperRddId = pRddNode->rddID;
   }

   /* Copy the non NULL entries from pSubTable into pTable */
   pFunction = ( DBENTRYP_V * ) pTable;
   pSubFunction = ( const DBENTRYP_V * ) pSubTable;
   for( uiCount = 0; uiCount < RDDFUNCSCOUNT; uiCount++ )
   {
      if( *pSubFunction )
         *pFunction = *pSubFunction;
      pFunction++;
      pSubFunction++;
   }
   return ZH_SUCCESS;
}

ZH_ERRCODE zh_rddInherit( RDDFUNCS * pTable, const RDDFUNCS * pSubTable,
                          RDDFUNCS * pSuperTable, const char * szDrvName )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_rddInherit(%p, %p, %p, %s)", ( void * ) pTable, ( const void * ) pSubTable, ( void * ) pSuperTable, szDrvName ) );

   return zh_rddInheritEx( pTable, pSubTable, pSuperTable, szDrvName, NULL );
}

ZH_BOOL zh_rddIsDerivedFrom( ZH_USHORT uiRddID, ZH_USHORT uiSuperRddID )
{
   if( uiRddID == uiSuperRddID )
      return ZH_TRUE;

   while( uiRddID < s_uiRddCount )
   {
      uiRddID = s_RddList[ uiRddID ]->rddSuperID;
      if( uiRddID == uiSuperRddID )
         return ZH_TRUE;
   }
   return ZH_FALSE;
}

/* extend the size of RDD nodes buffer to given value to avoid later
 * RT reallocations. It may be useful in some very seldom cases
 * for MT programs which will register dynamically at runtime
 * more then 128 RDDs.
 */
ZH_FUNC( __RDDPREALLOCATE )
{
   ZH_LONG lNewSize = zh_parnl( 1 );

   if( lNewSize > ( ZH_LONG ) USHRT_MAX )
      lNewSize = USHRT_MAX;
   if( lNewSize > ( ZH_LONG ) s_uiRddMax )
   {
      s_uiRddMax += ZH_RDD_POOL_ALLOCSIZE;
      s_RddList = ( LPRDDNODE * )
                  zh_xrealloc( s_RddList, sizeof( LPRDDNODE ) * s_uiRddMax );
   }

   zh_retnl( s_uiRddMax );
}

ZH_FUNC_EXTERN( RDDSYS );
extern void _zh_rddWorkAreaForceLink( void );
void _zh_rddWorkAreaForceLink( void )
{
   ZH_FUNC_EXEC( RDDSYS );
}
