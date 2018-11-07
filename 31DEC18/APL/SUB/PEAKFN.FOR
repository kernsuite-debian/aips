      SUBROUTINE PEAKFN (LUN, VOL, CNO, IDEPTH, CATBLK, BUFFER, JBUFSZ,
     *   PEAKX, PEAKY, IRET)
C-----------------------------------------------------------------------
C! returns location of maximum within 5 pixels of image plane center
C# Map-util
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
C   PEAKFN searches a region around the center of an image to locate the
C   pixel location of the maximum.  Will handle data cubes.
C   Inputs:
C      LUN      I        Logical unit number to use.
C      VOL      I        Disk on which image resides.
C      CNO      I        Catalog slot number of image.
C      IDEPTH   I(5)     Depth in image of desired plane.
C      CATBLK   I(256)   Catalog header block for image.
C      JBUFSZ   I        Size of the BUFFER in bytes
C   Output:
C      BUFFER   R(*)     Real work buffer
C      PEAKX    R        X coordinate of peak pixel location.
C      PEAKY    R        Y coordinate of peak pixel location.
C      IRET     I        Return code, 0=> OK, otherwise error.
C   Programmer: W. D. Cotton, Jan. 1983.
C-----------------------------------------------------------------------
      INTEGER   LUN, VOL, CNO, CATBLK(256), IDEPTH(5), IRET, JBUFSZ
      REAL      BUFFER(*), PEAKX, PEAKY
C
      INTEGER   NX, NY, BO, FIND, BIND, WIN(4), NS, IXMAX, IYMAX, I, J,
     *   IERR
      LOGICAL   T, F
      REAL      MAXVAL
      CHARACTER FILE*48
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA NS /5/
C-----------------------------------------------------------------------
      IRET = 1
C                                       Get NX, NY
      NX = CATBLK(KINAX)
      NY = CATBLK(KINAX+1)
C                                       Set window - search NS x NS.
      WIN(1) = NX / 2 + 1 - (NS / 2)
      WIN(2) = NY / 2 + 1 - (NS / 2)
      WIN(3) = WIN(1) + NS - 1
      WIN(4) = WIN(2) + NS - 1
C                                       Initialize maximum.
      MAXVAL = -1.0E20
      IXMAX = NX / 2 + 1
      IYMAX = NY / 2 + 1
C                                       Set block offset.
      CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), IDEPTH, BO, IERR)
      BO = BO + 1
C                                       Open image.
      CALL ZPHFIL ('MA', VOL, CNO, 1, FILE, IRET)
      CALL ZOPEN (LUN, FIND, VOL, FILE, T, F, T, IRET)
      IF (IRET.EQ.0) GO TO 50
         WRITE (MSGTXT,1000) IRET, VOL, CNO
         GO TO 990
C                                       Init. I/O
 50   CALL MINIT ('READ', LUN, FIND, NX, NY, WIN, BUFFER, JBUFSZ, BO,
     *   IRET)
      IF (IRET.EQ.0) GO TO 60
         WRITE (MSGTXT,1050) IRET, VOL, CNO
         GO TO 990
C                                       Find maximum: loop Y
 60   DO 200 I = 1,NS
C                                       Read row.
         CALL MDISK ('READ', LUN, FIND, BUFFER, BIND, IRET)
         IF (IRET.EQ.0) GO TO 100
            WRITE (MSGTXT,1060) IRET, VOL, CNO
            GO TO 990
C                                       Find max: loop X
 100     DO 140 J = 1,NS
C                                       New max.
            IF (MAXVAL.LT.BUFFER(BIND+J-1)) THEN
               MAXVAL = BUFFER(BIND+J-1)
               IXMAX = WIN(1) + J - 1
               IYMAX = WIN(2) + I - 1
               END IF
 140        CONTINUE
 200     CONTINUE
C                                       Close
      CALL ZCLOSE (LUN, FIND, IERR)
C                                       Set peak location.
      PEAKX = IXMAX
      PEAKY = IYMAX
      IRET = 0
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PEAKFN: ERROR',I3,' OPENING MAP VOL=',I3,' SLOT=',I4)
 1050 FORMAT ('PEAKFN: ERROR',I3,' INITING MAP VOL=',I3,' SLOT=',I4)
 1060 FORMAT ('PEAKFN: ERROR',I3,' READING MAP VOL=',I3,' SLOT=',I4)
      END
