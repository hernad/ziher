This directory contains compression related files including:
   * wrapper functions for minizip
   * some additionl functions to provide a higher-level API for ZIP files


Ziher functions to manage ZIP files:
======================================

zh_zipOpen( cFileName, [ iMode = ZH_ZIP_CREATE ],
            [ @cGlobalComment ] ) --> hZip
zh_zipClose( hZip, [ cGlobalComment ] ) --> nError
zh_zipFileCreate( hZip, cZipName, tDateTime, cTime,
                  nInternalAttr, nExternalAttr,
                  [ nMethod = ZH_ZLIB_METHOD_DEFLATE ],
                  [ nLevel = ZH_ZLIB_COMPRESSION_DEFAULT ],
                  [ cPassword, ulFileCRC32 ], [ cComment ] ) --> nError
zh_zipFileWrite( hZip, cData [, nLen ] ) --> nError
zh_zipFileClose( hZip ) --> nError
zh_zipStoreFile( hZip, cFileName, [ cZipName ], ;
                 [ cPassword ], [ cComment ] ) --> nError
zh_zipStoreFileHandle( hZip, fhnd, cZipName, ;
                 [ cPassword ], [ cComment ] ) --> nError
zh_zipFileCRC32( cFileName ) --> nError


zh_unzipOpen( cFileName ) --> hUnzip
zh_unzipClose( hUnzip ) --> nError
zh_unzipGlobalInfo( hUnzip, @nEntries, @cGlobalComment ) --> nError
zh_unzipFileFirst( hUnzip ) --> nError
zh_unzipFileNext( hUnzip ) --> nError
zh_unzipFilePos( hUnzip ) --> nPosition
zh_unzipFileGoto( hUnzip, nPosition ) --> nError
zh_unzipFileInfo( hUnzip, @cZipName, @tDateTime, @cTime,
                  @nInternalAttr, @nExternalAttr,
                  @nMethod, @nSize, @nCompressedSize,
                  @lCrypted, @cComment ) --> nError
zh_unzipFileOpen( hUnzip, [ cPassword ] ) --> nError
zh_unzipFileRead( hUnzip, @cBuf [, nLen ] ) --> nRead
zh_unzipFileClose( hUnzip ) --> nError
zh_unzipExtractCurrentFile( hUnzip, [ cFileName ], [ cPassword ] ) --> nError
zh_unzipExtractCurrentFileToHandle( hZip, fhnd, [ cPassword ] ) --> nError


zh_zipDeleteFile( cZipFile, cFileMask ) --> nError
