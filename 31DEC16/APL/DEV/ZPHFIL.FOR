      SUBROUTINE ZPHFIL (TYPE, IVOL, NSEQ, IVER, PNAM, IERR)
C-----------------------------------------------------------------------
C! construct a physical file or device name from AIPS logical parameters
C# Z IO-basic
C-----------------------------------------------------------------------
C;  Copyright (C) 1995
C;  Associated Universities, Inc. Washington DC, USA.
C;
C;  This program is free software; you can redistribute it and/or
C;  modify it under the terms of the GNU General Public License as
C;  published by the Free Software Foundation; either version 2 of
C;  the License, or (at your option) any later version.
C;
C;  This program is distributed in the hope that it will be useful,
C;  but WITHOUT ANY WARRANTY; without even the implied warranty of
C;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C;  GNU General Public License for more details.
C;
C;  You should have received a copy of the GNU General Public
C;  License along with this program; if not, write to the Free
C;  Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,
C;  MA 02139, USA.
C;
C;  Correspondence concerning AIPS should be addressed as follows:
C;         Internet email: aipsmail@nrao.edu.
C;         Postal address: AIPS Project Office
C;                         National Radio Astronomy Observatory
C;                         520 Edgemont Road
C;                         Charlottesville, VA 22903-2475 USA
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C   Construct a physical file name in PNAM from TYPE, IVOL, NSEQ, and
C   IVER - either for public data files or user-specific files.
C   Inputs:
C      TYPE    C*2   Type of file: e.g. 'MA' for map file
C      IVOL    I     Number of the disk volume to be used (1-15)
C      NSEQ    I     Sequence number (000-4095)
C      IVER    I     Version number (00-255)
C   Outputs:
C      PNAM    C*48  physical file name, left justified
C      IERR    I     Error return code: 0 = good return.   1 = error.
C
C   Example: If TYPE='MA', IVOL=7, AIPSVER=C, NSEQ=321, IVER=12799,
C            NLUSER=762 then
C               PNAME='DA06:MAD08X9VJ;'      for public data or
C               PNAME='DA06:MAD08X9VJ.0L6;'  for private data
C           use base 36 now: 321 => 08X, 12799 => 9VJ, 762 => 0L6
C           disks are base 36 but 0 relative
C   TYPE = 'MT' leads to special name for tapes
C   TYPE = 'TK' leads to special name for TEK4012 plotter CRT
C   TYPE = 'TV' leads to special name for TV device
C   TYPE = 'ME' leads to special logical for POPS memory files
C
C   Generic version - the ZOPEN, ZTRLOG, etc. routines interpret the
C   resulting VMS-like names.
C-----------------------------------------------------------------------
      CHARACTER TYPE*2, PNAM*48
      INTEGER   IVOL, NSEQ, IVER, IERR
C
      INTEGER   IVOL2, NUNAME, UNAME, VERLIM, VOLLIM, CATLIM
      CHARACTER JUNK*4, UNAMES(15)*2, VERDAT*1, VERSYS*1
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA NUNAME, UNAMES /15, 'AC','BA','BQ','HE','IC','ID','IN','ME',
     *   'SP','TP','TD','GR','PW','MT','TC'/
C                                       Current AIPS data version level
      DATA VERDAT, VERSYS /'D','D'/
C-----------------------------------------------------------------------
      IERR = 1
      PNAM = 'DA00:MADFFFFFF; '
      PNAM(8:8) = VERDAT
      IF (TYPE.EQ.'MT') PNAM(1:2) = 'MT'
C                                       Special code for TEK 4012
      IF (TYPE.EQ.'TK') THEN
         IF (NTKDEV.LE.0) GO TO 999
         PNAM = 'TKDEV  :'
         CALL ZEHEX (NTKDEV, 2, PNAM(6:7))
C                                       TV device
      ELSE IF (TYPE.EQ.'TV') THEN
         IF (NTVDEV.LE.0) GO TO 999
         PNAM = 'TVDEV  :'
         CALL ZEHEX (NTVDEV, 2, PNAM(6:7))
C                                       Special POPS memory files
      ELSE IF ((TYPE.EQ.'ME') .AND. (IVER.EQ.0)) THEN
         PNAM = VERNAM(1:3) // 'MEM:ME1000000;'
         PNAM(10:10) = VERSYS
C                                       other devices (disk files)
      ELSE
C                                       Check input parameters.
         VERLIM = 36 * 36 * 36 - 1
         CATLIM = VERLIM
         VOLLIM = 35
         IF ((IVOL.LT.1) .OR. (IVOL.GT.VOLLIM)) GO TO 999
         IF ((NSEQ.LT.0) .OR. (NSEQ.GT.CATLIM)) GO TO 999
         IF ((IVER.LT.0) .OR. (IVER.GT.VERLIM)) GO TO 999
C                                       User private?
            UNAME = 0
            IF (NUNAME.LE.0) GO TO 45
               DO 40 UNAME = 1,NUNAME
                  IF (TYPE.EQ.UNAMES(UNAME)) GO TO 45
 40               CONTINUE
               UNAME = 0
C                                       Adjust for 1 relative input
 45         IVOL2 = IVOL - 1
            IF (UNAME.LE.0) IVOL2 = IVOL
            CALL ZEHEX (IVOL2, 4, JUNK)
            PNAM(4:4) = JUNK(4:4)
            PNAM(6:7) = TYPE
            IF (UNAME.LE.0) THEN
               PNAM(8:8) = VERDAT
            ELSE
               PNAM(8:8) = VERSYS
               END IF
            CALL ZEHEX (NSEQ, 4, JUNK)
            PNAM(9:11) = JUNK(2:4)
            CALL ZEHEX (IVER, 4, JUNK)
            PNAM(12:14) = JUNK(2:4)
C                                       User private additions
            IF ((UNAME.LE.0) .AND. (UCTSIZ.GT.0)) THEN
               CALL ZEHEX (NLUSER, 4, JUNK)
               PNAM(15:) = '.' // JUNK(2:4) // ';'
               END IF
         END IF
C                                       No error
      IERR = 0
C
 999  RETURN
      END
