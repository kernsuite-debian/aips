      SUBROUTINE UCMPRS (NVIS, IVOL, ICNO, LUN, CATBLK, IERR)
C-----------------------------------------------------------------------
C! compresses the UV data set
C# IO-basic UV-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997
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
C   Compresses a UV data file to its minimum size.  The file must be
C   open and the header block completed except for the number of vis.
C   records.  That should be (for error messages only) the number that
C   set the current size of the file.
C   Inputs:
C      NVIS     I        Final number of visibilities
C      IVOL     I        Disk number
C      ICNO     I        Catalog number
C      LUN      I        LUN under which the file is open
C   In/out:
C      CATBLK   I(256)   Catalog header: NVIS put in group count on out
C   Output:
C      IERR     I        Error code: 0 okay, 1 inputs bad, 2 compress
C-----------------------------------------------------------------------
      INTEGER   NVIS, IVOL, ICNO, LUN, CATBLK(256), IERR
C
      INTEGER   LREC, I, N, SIZEI, SIZEO, SIZEA
      CHARACTER PHNAME*48
      REAL      X
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       get logical record length
      N = CATBLK(KIDIM)
      LREC = 1
      DO 10 I = 1,N
         LREC = LREC * MAX (1, CATBLK(KINAX+I-1))
 10      CONTINUE
      LREC = LREC + CATBLK(KIPCN)
C                                       Get file sizes
      CALL UVSIZE (LREC, CATBLK(KIGCN), SIZEI)
C                                       Put vis. count in CATBLK
      CATBLK(KIGCN) = NVIS
      CALL UVSIZE (LREC, CATBLK(KIGCN), SIZEO)
      SIZEA = SIZEO
C                                       File name
      CALL ZPHFIL ('UV', IVOL, ICNO, 1, PHNAME, IERR)
C                                       Compress it
      CALL ZCMPRS (IVOL, PHNAME, LUN, SIZEA, IERR)
      IF (IERR.NE.0) THEN
         IF (SIZEI.GT.SIZEO) THEN
            X = REAL (SIZEI) / REAL (SIZEO)
            WRITE (MSGTXT,1000) IERR, X
         ELSE
            WRITE (MSGTXT,1010) IERR
            END IF
         CALL MSGWRT (6)
         WRITE (MSGTXT,1015) IVOL, ICNO
         CALL MSGWRT (6)
      ELSE IF (SIZEA.LT.SIZEO) THEN
         WRITE (MSGTXT,1020) SIZEA, SIZEO
         CALL MSGWRT (6)
         WRITE (MSGTXT,1015) IVOL, ICNO
         CALL MSGWRT (6)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UCMPRS: ERROR',I3,' FILE LEFT',F5.2,' TIMES TOO BIG')
 1010 FORMAT ('UCMPRS: ERROR',I3,' RETURNED BY ZCMPRS')
 1015 FORMAT ('UCMPRS: ERROR WAS ON UV FILE, DISK',I3,' CATALOG NUMBER',
     *   I5)
 1020 FORMAT ('UCMPRS: GOT',I10,' BLOCKS, REQUESTED',I10,
     *   ' PROBABLE ERROR')
      END
