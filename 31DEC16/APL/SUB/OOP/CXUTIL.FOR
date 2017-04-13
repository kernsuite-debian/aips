C   CX_IMAGE Class utility module
C-----------------------------------------------------------------------
C! Object Oriented AIPS Fortran "CX_IMAGE" utility module.
C# MAP-util Utility Object-Oriented
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998
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
C   This package of utility routines is related to the complex image
C   (CX_IMAGE) class.  These mostly convert between CX_IMAGE objects
C   (the only type of complex image allowed permanently in AIPS) and
C   IMAGE objects of type complex (the useful ones for image
C   arithmetic).
C   Public functions:
C   IMG2CX (image, cximag, ierr)
C      Convert image of type complex to a CX_IMAGE.
C   CX2IMG (cximag, image, ierr)
C      Convert CX_IMAGE to image of type complex.
C   IMGR2C (rimage, cimage, ierr)
C      Convert real image to image of type complex with zero imaginary.
C-----------------------------------------------------------------------
LOCAL INCLUDE 'CXUSTUF.INC'
C                                       Local include for CXUTIL
      INCLUDE 'INCS:PMAD.INC'
      REAL     ROW(2*MAXIMG)
      COMMON /CXULCM/ ROW
LOCAL END
      SUBROUTINE IMG2CX (IMAGE, CXIMAG, IERR)
C-----------------------------------------------------------------------
C   CX_IMAGE class utility routine
C   Converts from an IMAGE of type complex (the type that's useful for
C   arithmetic)  to a complex CX_IMAGE (the type allowed permanantly in
C   AIPS).
C   Inputs:
C      IMAGE    C*?  Name of input IMAGE object.
C      CXIMAG   C*?  Name of output CX_IMAGE object.
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER IMAGE*(*), CXIMAG*(*)
      INTEGER   IERR
C
      INTEGER   TYPE, DIM(7), NAXIS(7), BLC(7), TRC(7), DUMMY,
     *   I2, I3, I4, I5, I6, I7
      LOGICAL   DOCMPL
      CHARACTER DATYPE*8
      INCLUDE 'CXUSTUF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PAOOF.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Open input.
      CALL IMGOPN (IMAGE, 'READ', IERR)
      IF (IERR.NE.0) GO TO 995
      CALL ARROPN (IMAGE, 'READ', IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Input better be complex
      CALL ARDGET (IMAGE, 'DATATYPE', TYPE, DIM, DUMMY, DATYPE, IERR)
      DOCMPL = (DATYPE.EQ.'COMPLEX') .AND. (IERR.EQ.0)
      IERR = 0
      IF (.NOT.DOCMPL) THEN
         IERR = 5
         MSGTXT = 'IMG2CX: INPUT NOT A COMPLEX IMAGE'
         GO TO 990
         END IF
C                                       Window
      CALL ARRWIN (IMAGE, BLC, TRC, NAXIS, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Open output.
      CALL CIMOPN (CXIMAG, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Loop over array
      DO 700 I7 = BLC(7),TRC(7)
         DO 600 I6 = BLC(6),TRC(6)
            DO 500 I5 = BLC(5),TRC(5)
               DO 400 I4 = BLC(4),TRC(4)
                  DO 300 I3 = BLC(3),TRC(3)
                     DO 200 I2 = BLC(2),TRC(2)
C                                       Read
      CALL ARREAD (IMAGE, DIM, ROW, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Write
      DIM(1) = DIM(1) / 2
      CALL CIPUTX (CXIMAG, DIM, ROW, IERR)
      IF (IERR.NE.0) GO TO 995
 200                    CONTINUE
 300                 CONTINUE
 400              CONTINUE
 500           CONTINUE
 600        CONTINUE
 700     CONTINUE
C                                       Close
      CALL ARRCLO (IMAGE, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL IMGCLO (IMAGE, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL CIMCLO (CXIMAG, IERR)
      IF (IERR.NE.0) GO TO 995
      GO TO 999
C                                       Error
 990  CALL MSGWRT (7)
 995  MSGTXT = 'IMG2CX: ERROR COPYING ' // IMAGE
      CALL MSGWRT (7)
      MSGTXT = 'TO ' // CXIMAG
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE CX2IMG (CXIMAG, IMAGE, IERR)
C-----------------------------------------------------------------------
C   CX_IMAGE class utility routine
C   Converts from a complex CX_IMAGE (the type allowed permanantly in
C   AIPS) to an IMAGE of type complex (the type that's useful for
C   arithmetic).
C   Inputs:
C      CXIMAG   C*?  Name of Input CX_IMAGE object.
C      IMAGE    C*?  Name of output IMAGE object.  Created as a scratch
C                    object if necessary.
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER CXIMAG*(*), IMAGE*(*)
      INTEGER   IERR
C
      INTEGER   TYPE, DIM(7), NAXIS(7), BLC(7), TRC(7), DUMMY,
     *   I2, I3, I4, I5, I6, I7
      LOGICAL   EXIST
      CHARACTER CXIR*32, DATYPE*8
      INCLUDE 'CXUSTUF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PAOOF.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Open input.
      CALL CIMOPN (CXIMAG, 'READ', IERR)
      IF (IERR.NE.0) GO TO 995
C                                        Get names of component parts.
      CALL OGET (CXIMAG, 'REALPART', TYPE, DIM, DUMMY, CXIR, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Window
      CALL ARRWIN (CXIR, BLC, TRC, NAXIS, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Create output if necessary
      CALL OBFEXS (IMAGE, EXIST, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (.NOT.EXIST) THEN
         IF (IMAGE.EQ.'        ') IMAGE = 'Scratch object in CX2IMG'
         NAXIS(1) = NAXIS(1) * 2
         CALL  IMGSCR (IMAGE, NAXIS, IERR)
         IF (IERR.NE.0) GO TO 995
         END IF
C                                       Output complex
      DATYPE = 'COMPLEX'
      DIM(1) = LEN (DATYPE)
      DIM(2) = 1
      CALL ARDPUT (IMAGE, 'DATATYPE', OOACAR, DIM, DUMMY, DATYPE, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Open output.
      CALL IMGOPN (IMAGE, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 995
      CALL ARROPN (IMAGE, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Loop over array
      DO 700 I7 = BLC(7),TRC(7)
         DO 600 I6 = BLC(6),TRC(6)
            DO 500 I5 = BLC(5),TRC(5)
               DO 400 I4 = BLC(4),TRC(4)
                  DO 300 I3 = BLC(3),TRC(3)
                     DO 200 I2 = BLC(2),TRC(2)
C                                       Read
      CALL CIGETX (CXIMAG, DIM, ROW, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Write
      DIM(1) = DIM(1) * 2
      CALL ARRWRI (IMAGE, DIM, ROW, IERR)
      IF (IERR.NE.0) GO TO 995
 200                    CONTINUE
 300                 CONTINUE
 400              CONTINUE
 500           CONTINUE
 600        CONTINUE
 700     CONTINUE
C                                       Close
      CALL CIMCLO (CXIMAG, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL ARRCLO (IMAGE, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL IMGCLO (IMAGE, IERR)
      IF (IERR.NE.0) GO TO 995
      GO TO 999
C                                       Error
 990  CALL MSGWRT (7)
 995  MSGTXT = 'CX2IMG: ERROR COPYING ' // CXIMAG
      CALL MSGWRT (7)
      MSGTXT = 'TO ' // IMAGE
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE IMGR2C (RIMAGE, CIMAGE, IERR)
C-----------------------------------------------------------------------
C   CX_IMAGE class utility routine
C   Converts from a real image to an image of type complex zeroing the
C   imaginary parts.
C   Inputs:
C      RIMAGE   C*?  Name of Input real object.
C      CIMAGE   C*?  Name of output complex object.  Created as a
C                    scratch object if necessary.
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER RIMAGE*(*), CIMAGE*(*)
      INTEGER   IERR
C
      INTEGER   DIM(7), NAXIS(7), BLC(7), TRC(7), DUMMY, LROW, J1, K1,
     *   I1, I2, I3, I4, I5, I6, I7
      LOGICAL   EXIST
      CHARACTER DATYPE*8
      INCLUDE 'CXUSTUF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PAOOF.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Open input.
      CALL IMGOPN (RIMAGE, 'READ', IERR)
      IF (IERR.NE.0) GO TO 995
      CALL ARROPN (RIMAGE, 'READ', IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Window
      CALL ARRWIN (RIMAGE, BLC, TRC, NAXIS, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Create output if necessary
      CALL OBFEXS (CIMAGE, EXIST, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (.NOT.EXIST) THEN
         IF (CIMAGE.EQ.'        ') CIMAGE = 'Scratch object in CX2IMG'
         NAXIS(1) = NAXIS(1) * 2
         CALL  IMGSCR (CIMAGE, NAXIS, IERR)
         IF (IERR.NE.0) GO TO 995
         END IF
C                                       Output complex
      DATYPE = 'COMPLEX'
      DIM(1) = LEN (DATYPE)
      DIM(2) = 1
      CALL ARDPUT (CIMAGE, 'DATATYPE', OOACAR, DIM, DUMMY, DATYPE, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Open output.
      CALL IMGOPN (CIMAGE, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 995
      CALL ARROPN (CIMAGE, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Loop over array
      LROW = TRC(1) - BLC(1) + 1
      DO 700 I7 = BLC(7),TRC(7)
         DO 600 I6 = BLC(6),TRC(6)
            DO 500 I5 = BLC(5),TRC(5)
               DO 400 I4 = BLC(4),TRC(4)
                  DO 300 I3 = BLC(3),TRC(3)
                     DO 200 I2 = BLC(2),TRC(2)
C                                       Read
      CALL ARREAD (RIMAGE, DIM, ROW, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Shuffle and zero
      DO 150 I1 = 2,LROW
         K1 = LROW - I1 + 2
         J1 = ((K1 - 1) * 2) + 1
         ROW(J1) = ROW(K1)
         ROW(J1+1) = 0.0
 150     CONTINUE
      ROW(2) = 0.0
C                                       Write
      DIM(1) = DIM(1) * 2
      CALL ARRWRI (CIMAGE, DIM, ROW, IERR)
      IF (IERR.NE.0) GO TO 995
 200                    CONTINUE
 300                 CONTINUE
 400              CONTINUE
 500           CONTINUE
 600        CONTINUE
 700     CONTINUE
C                                       Close
      CALL ARRCLO (RIMAGE, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL ARRCLO (CIMAGE, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL IMGCLO (RIMAGE, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL IMGCLO (CIMAGE, IERR)
      IF (IERR.NE.0) GO TO 995
      GO TO 999
C                                       Error
 990  CALL MSGWRT (7)
 995  MSGTXT = 'IMGR2C: ERROR COPYING ' // RIMAGE
      CALL MSGWRT (7)
      MSGTXT = 'TO ' // CIMAGE
      CALL MSGWRT (7)
C
 999  RETURN
      END
