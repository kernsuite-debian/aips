      PROGRAM PADIM
C-----------------------------------------------------------------------
C! Pads an image with an arbitrary value.
C# Map
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2008-2010
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
C   PADIM: make an image bigger by padding it with a number of your
C   choice
C RPARM    INPUTS:
C  1        USERID               Owner of the image
C  2-4      INNAME(3)            Image name (name)
C  5-6      INCLASS(2)           Image name (class)
C  7        INSEQ                Image name (seq. #)
C  8        INDISK               Disk # of image
C  9-11     OUTNAME(3)           Output image name (name)
C  12-13    OUTCLASS(2)          Output image name (class)
C  14       OUTSEQ               Output image name (seq. #)
C  15       OUTDISK              Disk # of output image
C  16-17    IMSIZE(2)            Output image size
C  18-27    CPARM(10)            User supplied parameters, 1-2 used
C
C     Neil Killeen, April 1987
C-----------------------------------------------------------------------
      CHARACTER INNAM*36, OUTNAM*36
      REAL      RPARM(27), PADVAL
      INTEGER   NROWIN, NCOLIN
      INTEGER   LIN, LOUT, NPARMS, IERR, IER, INSL, OUTSL, INVOL,
     *   OUTVOL, IMSIZE(3)
C
      DATA LIN, LOUT, NPARMS /17, 18, 27/
C-----------------------------------------------------------------------
C                                       Start up task & get inputs
      CALL START (NPARMS, RPARM, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Extract user parameters
      CALL EXTRAC (RPARM, PADVAL, IMSIZE, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Open input & output images
      CALL OPENIM (RPARM, LIN, LOUT, IMSIZE, INNAM, OUTNAM, INSL,
     *   OUTSL, INVOL, OUTVOL, NCOLIN, NROWIN, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Compute output image
      CALL OUTPUT (NCOLIN, NROWIN, IMSIZE, LIN, LOUT, PADVAL,
     *   OUTSL, OUTVOL, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Add history file
      CALL PADHIS (INNAM, OUTNAM, INSL, OUTSL, INVOL, OUTVOL,
     *   IMSIZE, PADVAL, IERR)
C
 900  IF (IERR.EQ.0) THEN
         IER = 0
      ELSE
         IER = 1
         END IF
      CALL CLENUP
      CALL TSKEND (IER)
C
      STOP
      END
      SUBROUTINE START (NPARMS, RPARM, IERR)
C-----------------------------------------------------------------------
C     Start up the task and get AIPS inputs
C
C     INPUT:
C       NPARMS   I       Number of AIPS parameters
C     OUTPUT:
C       RPARM    R       AIPS input parameters
C       IERR     I       0 => OK
C
C-----------------------------------------------------------------------
      REAL   RPARM(*)
      INTEGER   NPARMS, IERR
      CHARACTER PRGNAM*6
C
      INCLUDE 'INCS:DMSG.INC'
C
      DATA PRGNAM /'PADIM '/
C-----------------------------------------------------------------------
C                                       dropping USERID in inputs
      CALL TSKBEG (PRGNAM, NPARMS-1, RPARM(2), IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
      ELSE
         WRITE (MSGTXT,2000)
         END IF
      CALL MSGWRT (8)
      RPARM(1) = NLUSER
C
      RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('START: COULD NOT GET INPUTS, IERR=', I3)
 2000 FORMAT ('PADIM IS A NON-STANDARD PROGRAM')
      END
      SUBROUTINE EXTRAC (RPARM, PADVAL, IMSIZE, IERR)
C-----------------------------------------------------------------------
C   Extract the user supplied parameters from the INPUTS
C   Input:
C      RPARM    R      AIPS inputs
C   Output:
C      PADVAL   R      Value to pad image with, 'INDE' allowed
C      IMSIZE   I      Output image image size as supplied by user
C      IERR     I      0 => OK
C-----------------------------------------------------------------------
      REAL   RPARM(*), PADVAL
      INTEGER   IMSIZE(3), IERR
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Get output image size
      IERR = 0
      IMSIZE(1) = RPARM(16) + 0.01
      IMSIZE(2) = RPARM(17) + 0.01
      IF ((IMSIZE(1).LE.0) .OR. (IMSIZE(2).LE.0)) THEN
         WRITE (MSGTXT,1000)
         CALL MSGWRT (8)
         IERR = 1
         GO TO 999
         END IF
C                                       Get padding value
      IF (RPARM(19).GT.0.0) THEN
         PADVAL = FBLANK
         WRITE (MSGTXT,2000)
      ELSE
         PADVAL = RPARM(18)
         WRITE (MSGTXT,3000) PADVAL
         END IF
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('EXTRAC: OUTPUT IMAGE MUST BE POSITIVE')
 2000 FORMAT ('EXTRAC: Padding image with magic blanks')
 3000 FORMAT ('EXTRAC: Padding image with ', 1PE12.5, '''s')
      END
      SUBROUTINE OPENIM (RPARM, LIN, LOUT, IMSIZE, INNAM, OUTNAM, INSL,
     *   OUTSL, INVOL, OUTVOL, NCOLIN, NROWIN, IERR)
C-----------------------------------------------------------------------
C     Open the input image, create and open the output image.
C     Input:
C         RPARM     R       Array of AIPS input parameters
C         LIN       I       LUN for input image
C         LOUT      I       LUN for output image
C     Input/output:
C         IMSIZE    I       Output image size
C     Output:
C         INNAM     C*36    Namestring for input image
C         OUTNAM    C*36    Namestring for output image
C         INSL      I       Cat slot of input image
C         OUTSL     I       Cat slot of output image
C         INVOL     I       Volume number of input image
C         OUTVOL    I       Volume number of output image
C         NCOLIN    I       Number of columns in input image
C         NROWIN    I       Number of rows in input image
C         IERR      I       Error status, 0=> OK
C-----------------------------------------------------------------------
      CHARACTER INNAM*36, OUTNAM*36
      REAL      RPARM(*)
      INTEGER   NROWIN, NCOLIN
      INTEGER   LIN, LOUT, IERR, INSL, OUTSL, OUTVOL, INVOL, IMSIZE(3)
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Open input image
      CALL OPENIN (RPARM, LIN, INNAM, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 900
         END IF
C                                       Get catalog slot
      CALL CATSLV (LIN, INSL, INVOL, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,2000) IERR
         GO TO 900
         END IF
C                                       Now check output image size
      CALL CHKIMS (IMSIZE, NCOLIN, NROWIN, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,3000) IERR
         GO TO 900
         END IF
C                                       Create and open output image
      CALL OPENOU (RPARM, IMSIZE, LOUT, INNAM, OUTNAM, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,4000) IERR
         GO TO 900
         END IF
C                                       Get catalog slot
      CALL CATSLV (LOUT, OUTSL, OUTVOL, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,5000) IERR
         GO TO 900
         END IF
C                                       copy keywords
      CALL KEYCOP (INVOL, INSL, OUTVOL, OUTSL, IERR)
      IERR = 0
C
 900  IF (IERR.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('OPENIM: COULD NOT OPEN INPUT IMAGE, IERR=', I3)
 2000 FORMAT ('OPENIM: COULD NOT FIND INPUT IMAGE CAT SLOT, IERR=', I3)
 3000 FORMAT ('OPENIM: ILLEGAL OUTPUT IMAGE SIZE, IERR = ', I3)
 4000 FORMAT ('OPENIM: COULD NOT OPEN OUTPUT IMAGE, IERR=', I3)
 5000 FORMAT ('OPENIM: COULD NOT FIND OUTPUT IMAGE CAT SLOT, IERR=', I3)
      END
      SUBROUTINE OPENIN (RPARM, LIN, INNAM, IERR)
C-----------------------------------------------------------------------
C     Set up the input namestring, open the input image and get the
C   header
C     INPUT:
C         RPARM     R       array of AIPS input parameters
C         LIN       I       LUN of input image
C     OUTPUT:
C         INNAM     C*36    Namestring for input image
C         IERR      I       error status, 0=> OK
C
C-----------------------------------------------------------------------
      CHARACTER INNAM*36
      HOLLERITH MAP
      REAL   CATIR(128), RPARM(*)
      INTEGER   LIN, CATIN(256), CATOUT(256), IERR
      INCLUDE 'INCS:DMSG.INC'
      COMMON /HEADS/ CATIN, CATOUT
      EQUIVALENCE (CATIN, CATIR)
C-----------------------------------------------------------------------
C                                       Set up input namestring
      CALL CHR2H (4, 'MA  ', 1, MAP)
      CALL H2WAWA (RPARM(2), RPARM(5), RPARM(7), MAP, RPARM(8),
     *   RPARM(1), INNAM)
C                                       Open input image
      CALL OPENCF (LIN, INNAM, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 900
         END IF
C                                       Get header
      CALL GETHDR (LIN, CATIN, IERR)
      IF (IERR.NE.0) WRITE (MSGTXT,2000) IERR
C
 900  IF (IERR.NE.0) CALL MSGWRT (8)
C
      RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('OPENIN: ERROR OPENING INPUT IMAGE, IERR=', I3)
 2000 FORMAT ('OPENIN: ERROR GETTING INPUT IMAGE HEADER, IERR=', I3)
      END
      SUBROUTINE CHKIMS (IMSIZE, NCOLIN, NROWIN, IERR)
C-----------------------------------------------------------------------
C   Check that the output image size is bigger than the input image
C   size
C     Input:
C       IMSIZE   I       Output image size
C     Output:
C       NCOLIN   I       Number of columns in input image
C       NROWIN   I       Number of rows in input image
C       IERR     I       0 => OK
C-----------------------------------------------------------------------
      INTEGER   NCOLIN, NROWIN
      INTEGER   CATIN(256), CATOUT(256), IERR, IMSIZE(3), NDIM, I
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PMAD.INC'
      COMMON /HEADS/ CATIN, CATOUT
C-----------------------------------------------------------------------
      NCOLIN = CATIN(KINAX)
      NROWIN = CATIN(KINAX+1)
      NDIM = CATIN(KIDIM)
      IMSIZE(1) = MIN (MAXIMG, IMSIZE(1))
      IMSIZE(2) = MIN (MAXIMG, IMSIZE(2))
      IF ((IMSIZE(1).LT.NCOLIN) .OR. (IMSIZE(2).LT.NROWIN)) THEN
         WRITE (MSGTXT,1000)
         IERR = 1
         END IF
      IF ((IMSIZE(1).EQ.NCOLIN) .AND. (IMSIZE(2).EQ.NROWIN)) THEN
         WRITE (MSGTXT,2000)
         IERR = 2
         END IF
      IF (IERR.NE.0) CALL MSGWRT (8)
      IMSIZE(3) = 1
      DO 10 I = 3,NDIM
         IMSIZE(3) = IMSIZE(3) * MAX (1, CATIN(KINAX+I-1))
 10      CONTINUE
C
      RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CHKIMS: AT LEAST 1 OUTPUT IMAGE DIM. IS SMALLER',
     *        ' THAN INPUT DIM.')
 2000 FORMAT ('CHKIMS: OUTPUT IMAGE MUST BE BIGGER THAN INPUT IMAGE')
      END
      SUBROUTINE CATSLV (LUN, SLOT, VOL, IERR)
C-----------------------------------------------------------------------
C     Get catalog slot and volume number for open image file specified
C     by its LUN
C     Input:
C       LUN      I       Logical unit number of already OPEN image
C    Output:
C       SLOT     I       Catalog slot of image
C       VOL      I       Volume number
C       IERR     I       Error status, 0 => OK
C-----------------------------------------------------------------------
      INTEGER   LUN, SLOT, VOL, IERR, I
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DITB.INC'
C-----------------------------------------------------------------------
C                                       Match LUN in FILTAB
      DO 100 I = 1,EFIL
         IF (FILTAB(POLUN, I).EQ.LUN) THEN
            SLOT = FILTAB(POCAT, I)
            VOL  = FILTAB(POVOL, I)
            IERR = 0
            GO TO 999
            END IF
 100     CONTINUE
C                                       Didn't find LUN in FILTAB
      WRITE (MSGTXT,1000) LUN
      CALL MSGWRT (8)
      IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CATSLV: CAN''T FIND LUN = ', I3, ' IN FILTAB')
      END
      SUBROUTINE OPENOU (RPARM, IMSIZE, LOUT, INNAM, OUTNAM, IERR)
C-----------------------------------------------------------------------
C     Create and open output image
C     Inputs:
C         RPARM     R       Array of AIPS input parameters
C         IMSIZE    I       Output image size
C         LOUT      I       LUN for output image
C         INNAM     C*36    Namestring for input image
C     Outputs:
C         OUTNAM    C*36    Namestring for output image
C         IERR      I       error status, 0=> OK
C
C-----------------------------------------------------------------------
      CHARACTER DEFNAM*36, INNAM*36, OUTNAM*36, NAME*12, CLASS*6,
     *   PTYPE*2
      HOLLERITH MAP
      REAL   RPARM(*)
      INTEGER   LOUT, IERR, CATIN(256), CATOUT(256), IMSIZE(3),
     *   ISEQ, IVOL, USID
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      COMMON /HEADS/ CATIN, CATOUT
C-----------------------------------------------------------------------
C                                       Output name string
      CALL CHR2H (4, 'MA  ', 1, MAP)
C                                       Default name
      CALL WAWA2A (INNAM, NAME, CLASS, ISEQ, PTYPE, IVOL, USID)
      CALL A2WAWA (NAME, '      ', ISEQ, PTYPE, IVOL, USID, DEFNAM)
C                                       Requested name
      CALL H2WAWA (RPARM(9), RPARM(12), RPARM(14), MAP, RPARM(15),
     *   RPARM(1), OUTNAM)
C                                       Create output image header
      CALL HEADER (IMSIZE)
C                                       Create output image
      CALL MAPCR (DEFNAM, OUTNAM, CATOUT, IERR)
      IF (IERR.NE.0) THEN
        WRITE (MSGTXT,1000) IERR
        GO TO 900
         END IF
C                                       Open output image
      CALL OPENCF (LOUT, OUTNAM, IERR)
      IF (IERR.NE.0) WRITE (MSGTXT,2000) IERR
C
 900  IF (IERR.NE.0) CALL MSGWRT (8)
C
      RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('OPENOU: COULD NOT CREATE OUTPUT IMAGE, IERR=', I3)
 2000 FORMAT ('OPENOU: COULD NOT OPEN OUTPUT IMAGE, IERR=',  I3)
      END
      SUBROUTINE HEADER (IMSIZE)
C-----------------------------------------------------------------------
C     Make the output image header.
C     Input:
C         IMSIZE(3) I       Output image size
C-----------------------------------------------------------------------
      DOUBLE PRECISION CATID(128), CATOD(128)
      REAL      CATOR(256)
      INTEGER   I, NCOLIN, NROWIN, ILEFT, IBOTT
      INTEGER   CATIN(256), CATOUT(256), IMSIZE(3)
      INCLUDE 'INCS:DHDR.INC'
      COMMON /HEADS/ CATIN, CATOUT
      EQUIVALENCE (CATIN, CATID), (CATOUT, CATOR, CATOD)
C-----------------------------------------------------------------------
C                                       Copy old to new
      DO 100 I = 1,256
         CATOUT(I) = CATIN(I)
 100     CONTINUE
C                                       Input and output dimensions
      NCOLIN = CATIN(KINAX)
      NROWIN = CATIN(KINAX+1)
      CATOUT(KINAX) = IMSIZE(1)
      CATOUT(KINAX+1) = IMSIZE(2)
C                                       Shift reference pixel
      ILEFT = (IMSIZE(1) - NCOLIN) / 2
      IBOTT = (IMSIZE(2) - NROWIN) / 2
      CATOR(KRCRP) = CATOR(KRCRP) + ILEFT
      CATOR(KRCRP+1) = CATOR(KRCRP+1) + IBOTT
C
 999  RETURN
      END
      SUBROUTINE OUTPUT (NCOLIN, NROWIN, IMSIZE, LIN, LOUT, PADVAL,
     *   OUTSL, OUTVOL, IERR)
C-----------------------------------------------------------------------
C     Main subroutine to pad the image
C     Input:
C       NCOLIN     I     Number of columns in output image
C       NROWIN     I     Number of rows in output image
C       IMSIZE(3)  I     Output image size
C       LIN,LOUT   I     LUNS for input and output files
C       PADVAL     R     Value to pad with, can be 'INDE'
C       OUTSL      I     Output image cat slot
C       OUTVOL     I     Output image volume number
C     Output:
C       IERR       I     0 => OK
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PMAD.INC'
      REAL      RBUF(MAXIMG), CATOR(128), PADVAL
      INTEGER   NROWIN, NCOLIN, ILEFT, IBOTT, ITOP, J, IPL
      INTEGER   LIN, LOUT, IERR, OUTSL, OUTVOL, CATIN(256),
     *   CATOUT(256), IBUFF(256), IMSIZE(3)
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      COMMON /HEADS/ CATIN, CATOUT
      EQUIVALENCE (CATOUT, CATOR)
C-----------------------------------------------------------------------
C                                       # padding pixels, left, bottom,
C                                       top
      ILEFT = (IMSIZE(1) - NCOLIN) / 2
      IBOTT = (IMSIZE(2) - NROWIN) / 2
      ITOP  = IMSIZE(2) - IBOTT - NROWIN
C                                       Loop over all planes
      DO 100 IPL = 1,IMSIZE(3)
C                                       Bottom of output to bottom of
C                                       input
         IF (IBOTT.GE.1) THEN
            CALL RFILL (IMSIZE(1), PADVAL, RBUF)
            DO 20 J = 1,IBOTT
               CALL MAPIO ('WRIT', LOUT, RBUF, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1000) J, IERR
                  CALL MSGWRT (8)
                  GO TO 900
                  END IF
 20            CONTINUE
            END IF
C                                       Bottom of input to top of input
         DO 30 J = IBOTT + 1, IBOTT + NROWIN
            CALL MAPIO ('READ', LIN, RBUF(ILEFT+1), IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,2000) J, IERR
               CALL MSGWRT (8)
               GO TO 900
               END IF
            CALL MAPIO ('WRIT', LOUT, RBUF, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) J, IERR
               CALL MSGWRT (8)
               GO TO 900
               END IF
 30         CONTINUE
C                                       Top of input to top of output
         IF (ITOP.GE.1) THEN
            CALL RFILL (IMSIZE(1), PADVAL, RBUF)
            DO 50 J = IBOTT + NROWIN + 1, IMSIZE(2)
               CALL MAPIO ('WRIT', LOUT, RBUF, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1000) J, IERR
                  CALL MSGWRT (8)
                  GO TO 900
                  END IF
 50            CONTINUE
            END IF
 100     CONTINUE
C                                       Update min and max
      IF (PADVAL.NE.FBLANK) THEN
         CATOR(KRDMN) = MIN (PADVAL, CATOR(KRDMN))
         CATOR(KRDMX) = MAX (PADVAL, CATOR(KRDMX))
         END IF
C                                       Update header
      IF (PADVAL.EQ.FBLANK) CATOR(KRBLK) = FBLANK
      CALL CATIO ('UPDT', OUTVOL, OUTSL, CATOUT, 'REST', IBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,3000) IERR
         GO TO 900
         END IF
C                                       Close image files
      CALL FILCLS (LIN)
      CALL FILCLS (LOUT)
C
 900  IF (IERR.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('OUTPUT: COULD NOT WRITE LINE # ', I4, ' IERR=', I3)
 2000 FORMAT ('OUTPUT: COULD NOT READ LINE # ', I4, ' IERR=', I3)
 3000 FORMAT ('OUTPUT: COULD NOT UPDATE HEADER BLOCK, IERR=', I3)
      END
      SUBROUTINE PADHIS (INNAM, OUTNAM, INSL, OUTSL, INVOL, OUTVOL,
     *   IMSIZE, PADVAL, IERR)
C-----------------------------------------------------------------------
C    PADHIS writes the history file for the task PADIM
C    Inputs:
C        INNAM            C*36 Input map namestring
C        OUTNAM           C*36  Output map namestring
C        INSL             I     Slot number for input map
C        OUTSL            I     Slot number for output map
C        INVOL            I     Input image volume number
C        OUTVOL           I     Output image volume number
C        IMSIZE           I     Output image size
C        PADVAL           R     Padding value
C   Outputs:
C        IERR             I     Error return
C                               0-> okay
C                               1-> no good
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6, INNAM*36, OUTNAM*12, HILINE*72,
     *   NAME*12, CLASS*6, PTYPE*2
      REAL      PADVAL
      INTEGER   IERR, NHISTF, LHIN, LHOUT, INSL, OUTSL, INVOL,
     *   OUTVOL, IBUFF1(256), IBUFF2(256),
     *   CATIN(256), CATOUT(256), IMSIZE(3), SEQ, VOL, USID
      LOGICAL   T
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DDCH.INC'
      COMMON /HEADS/ CATIN, CATOUT
      DATA NHISTF, LHIN, LHOUT /2, 27, 28/
      DATA PRGNAM /'PADIM '/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Init HI
      CALL HIINIT (NHISTF)
C                                       Copy old to new
      CALL HISCOP (LHIN, LHOUT, INVOL, OUTVOL, INSL, OUTSL, CATOUT,
     *   IBUFF1, IBUFF2, IERR)
      IF (IERR.GE.3) THEN
        WRITE (MSGTXT,1000) IERR
        GO TO 900
      ELSE
        IERR = 0
         END IF
C                                       Add INNAME
      CALL WAWA2A (INNAM, NAME, CLASS, SEQ, PTYPE, VOL, USID)
      CALL HENCO1 (PRGNAM, NAME, CLASS, SEQ, VOL, LHOUT, IBUFF2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,2000) IERR
         GO TO 900
         END IF
C                                       Add OUTNAME
      CALL WAWA2A (OUTNAM, NAME, CLASS, SEQ, PTYPE, VOL, USID)
      CALL HENCOO (PRGNAM, NAME, CLASS, SEQ, VOL, LHOUT, IBUFF2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,3000) IERR
         GO TO 900
         END IF
C                                       Add imsize
      WRITE (HILINE,4000) PRGNAM, IMSIZE(1), IMSIZE(2)
      CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,5000) IERR
         GO TO 900
         END IF
C                                       Add padding value
      IF (PADVAL.EQ.FBLANK) THEN
         WRITE (HILINE,6000) PRGNAM
      ELSE
         WRITE (HILINE,7000) PRGNAM, PADVAL
         END IF
      CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,8000) IERR
         GO TO 900
         END IF
C                                       Close history file
      CALL HICLOS (LHOUT, T, IBUFF2, IERR)
      IF (IERR.NE.0) WRITE (MSGTXT,9000) IERR
C
 900  IF (IERR.NE.0) THEN
         CALL MSGWRT (8)
C                                        Copy tables
      ELSE
         CALL ALLTAB (1, ' ', LHIN, LHOUT, INVOL, OUTVOL, INSL, OUTSL,
     *      CATOUT, IBUFF1, IBUFF2, IERR)
         IF (IERR.GT.2) THEN
            MSGTXT = 'ERROR COPYING TABLE FILES'
            CALL MSGWRT (6)
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PADHIS: COULD NOT COPY OLD HISTORY FILE TO NEW, IERR=',
     *        I3)
 2000 FORMAT ('PADHIS: COULD NOT WRITE INNAME TO HISTORY FILE, IERR=',
     *        I3)
 3000 FORMAT ('PADHIS: COULD NOT WRITE OUTNAME TO HISTORY FILE, IERR=',
     *        I3)
 4000 FORMAT (A6, 'IMSIZE = ', I4, ',', I4)
 5000 FORMAT ('PADHIS: COULD NOT ADD IMAGE SIZE TO HISTORY IERR=', I3)
 6000 FORMAT (A6, '/ Image padded with magic blanks')
 7000 FORMAT (A6, '/ Image padded with ', 1PE11.4, '''s')
 8000 FORMAT ('PADHIS: COULD NOT ADD PADDING VALUE TO HISTORY, IERR=',
     *        I3)
 9000 FORMAT ('PADHIS: COULD NOT CLOSE HISTORY FILE, IERR=', I3)
      END
