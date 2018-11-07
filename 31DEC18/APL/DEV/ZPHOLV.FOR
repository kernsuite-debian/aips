      SUBROUTINE ZPHOLV (VERDAT, VERSYS, TYPE, IVOL, NSEQ, IVER, PNAM,
     *   IERR)
C-----------------------------------------------------------------------
C! construct a physical file - version for UPDAT
C# Z IO-basic
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2017
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
C   Construct a physical file name in PNAM from TYPE, IVOL, NSEQ, and
C   IVER - either for public data files or user-specific files.
C   Version of ZPHFIL for UPDAT, in which the Format version codes are
C   passed as arguments.
C   Inputs:
C      VERDAT  C*1   Data file version code letter
C      VERSYS  C*1   System file version code letter
C      TYPE    C*2   Type of file: e.g. 'MA' for map file
C      IVOL    I     Number of the disk volume to be used (1-71)
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
C
C   TYPE = 'MT' leads to special name for tapes
C   TYPE = 'TK' leads to special name for TEK4012 plotter CRT
C   TYPE = 'TV' leads to special name for TV device
C   TYPE = 'ME' leads to special logical for POPS memory files
C
C   Generic version - the ZOPEN, ZTRLOG, etc. routines interpret the
C   resulting VMS-like names.
C   If VERDAT = D or greater, use base 36 names, else use base 16.
C-----------------------------------------------------------------------
      CHARACTER VERDAT*1, VERSYS*1, TYPE*2, PNAM*48
      INTEGER   IVOL, NSEQ, IVER, IERR
C
      INTEGER   IVOL2, NUNAME, UNAME, VERLIM, VOLLIM, CATLIM
      CHARACTER CHVOL*4, CHVER*4, CHCAT*4, CHUSER*4, UNAMES(15)*2
      LOGICAL   NEW
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA NUNAME, UNAMES /15, 'AC','BA','BQ','HE','IC','ID','IN','ME',
     *   'SP','TP','TD','GR','PW','MT','TC'/
C-----------------------------------------------------------------------
      NEW = (VERDAT.NE.'A') .AND. (VERDAT.NE.'B') .AND. (VERDAT.NE.'C')
      IERR = 1
      IF (NEW) THEN
         PNAM = 'DA00:MAAFFFFFF;1 '
         VERLIM = 36 * 36 * 36 - 1
         CATLIM = VERLIM
         VOLLIM = 99
      ELSE
         PNAM = 'DA00:MAAFFFFF;1 '
         VERLIM = 16 * 16 - 1
         CATLIM = 16 * 16 * 16 - 1
         VOLLIM = 15
         END IF
      PNAM(8:8) = VERDAT
      IF (TYPE.EQ.'MT') PNAM(1:2) = 'MT'
C                                       Special code for TEK 4012
      IF (TYPE.EQ.'TK') THEN
         IF (NTKDEV.LE.0) GO TO 999
         PNAM = 'TKDEV00:'
         CALL ZEHEX (NTKDEV, 2, PNAM(6:7))
C                                       TV device
      ELSE IF (TYPE.EQ.'TV') THEN
         IF (NTVDEV.LE.0) GO TO 999
         PNAM = 'TVDEV00:'
         CALL ZEHEX (NTVDEV, 2, PNAM(6:7))
C                                       Special POPS memory files
      ELSE IF (TYPE.EQ.'ME') THEN
         IF ((IVER.LT.0) .OR. (IVER.GT.VERLIM)) GO TO 999
            IF (NEW) THEN
               PNAM = VERNAM(1:3) // 'MEM:ME1000000;1'
               CALL ZEHEX (IVER, 3, CHVER)
               PNAM(14:16) = CHVER(1:3)
            ELSE
               PNAM = VERNAM(1:3) // 'MEM:ME100000;1'
               CALL ZHEX (IVER, 2, CHVER)
               PNAM(14:15) = CHVER(1:2)
               END IF
            PNAM(10:10) = VERSYS
C                                       other devices (disk files)
      ELSE
C                                       Check input parameters.
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
            IF (NEW) THEN
               CALL ZEHEX (IVOL2, 4, CHVOL)
               CALL ZEHEX (NSEQ, 4, CHCAT)
               CALL ZEHEX (IVER, 4, CHVER)
               CALL ZEHEX (NLUSER, 4, CHUSER)
            ELSE
               CALL ZHEX (IVOL2, 4, CHVOL)
               CALL ZHEX (NSEQ, 4, CHCAT)
               CALL ZHEX (IVER, 4, CHVER)
               CALL ZHEX (NLUSER, 4, CHUSER)
               END IF
C                                       2 digit volume numbers!
            PNAM(3:4) = CHVOL(3:4)
            PNAM(6:7) = TYPE
            IF (UNAME.LE.0) THEN
               PNAM(8:8) = VERDAT
            ELSE
               PNAM(8:8) = VERSYS
               END IF
            PNAM(9:11) = CHCAT(2:4)
            IF (NEW) THEN
               PNAM(12:14) = CHVER(2:4)
            ELSE
               PNAM(12:13) = CHVER(3:4)
               END IF
C                                       User private additions
            IF ((UNAME.LE.0) .AND. (UCTSIZ.GT.0)) THEN
               IF (NEW) THEN
                  PNAM(15:) = '.' // CHUSER(2:4) // ';'
               ELSE
                  PNAM(14:) = '.' // CHUSER(2:4) // ';1'
                  END IF
               END IF
         END IF
C                                       No error
      IERR = 0
C
 999  RETURN
      END
