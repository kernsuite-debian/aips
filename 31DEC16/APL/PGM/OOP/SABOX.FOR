LOCAL INCLUDE 'INPUT.INC'
C                                       Declarations for inputs
      INTEGER   NPARMS
      PARAMETER (NPARMS=12)
      INTEGER   AVTYPE(NPARMS), AVDIM(2,NPARMS)
      CHARACTER AVNAME(NPARMS)*8
LOCAL END
LOCAL INCLUDE 'INPUTDATA.INC'
C                                       DATA statments defining input
C                                       parameters.
      INCLUDE 'INCS:PAOOF.INC'
      DATA AVNAME /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'NFIELD',
     *   'NGAUSS', 'BLC', 'TRC', 'BOXFILE', 'OBOXFILE', 'APARM',
     *   'BADDISK'/
      DATA AVTYPE /OOACAR, OOACAR, OOAINT, OOAINT, OOAINT,
     *   OOAINT, OOAINT, OOAINT, OOACAR, OOACAR, OOARE, OOAINT/
      DATA AVDIM /12,1, 6,1, 1,1, 1,1, 1,1,
     *   1,1, 7,1, 7,1, 48,1, 48,1, 10,1, 10,1/
LOCAL END
      PROGRAM SABOX
C-----------------------------------------------------------------------
C! find boxes to clean existing images
C# Task IMAGING OOP
C-----------------------------------------------------------------------
C;  Copyright (C) 2009, 2012
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
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PCLN.INC'
      CHARACTER PRGM*6, IN(MAXFLD)*32
      INTEGER   NIMAGE, IRET, BUFF1(256), FNUM(MAXFLD), NBOX(MAXFLD),
     *   MAXDIM, I, WIN(4,MXNBFL)
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'SABOX '/
C-----------------------------------------------------------------------
C                                       Startup
      I = MAXFLD
      CALL SABXIN (PRGM, I, IN, NIMAGE, FNUM, MAXDIM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       find boxes
      CALL SABOXD (IN, NIMAGE, FNUM, MAXDIM, NBOX, WIN, IRET)
      IF (IRET.NE.0) GO TO 990
      IRET = 0
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE SABXIN (PRGN, NMMAX, IN, NIMAGE, FNUM, MAXDIM, IRET)
C-----------------------------------------------------------------------
C   SABXIN gets input parameters for SABOX.
C   Inputs:
C      PRGN     C*6      Program name
C      NMMAX    I        Max dimension of IN
C   Output:
C      IN       C(*)*?   Input object
C      NIMAGE   I        Number of images in IN
C      FNUM     I(*)     Facet numbers of images in IN
C      MAXDIM   I        Maximum X*Y dimension of window
C      IRET     I        Error code: 0 => ok
C-----------------------------------------------------------------------
      INTEGER   NMMAX, NIMAGE, FNUM(*), MAXDIM, IRET
      CHARACTER PRGN*6, IN(*)*(*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NKEY1
C                                       NKEY1=no. adverbs for inname
      PARAMETER (NKEY1=5)
      INTEGER   DIM(7), TYPE, BLC(7), TRC(7), NAXIS(7), DUMMY, NFIELD,
     *   LFIELD, IDUMMY, MSGSAV, EDGSKP, J0, KFIELD, NP, NGAUSS, NMAPS
      REAL      APARM(10), ACTN
      CHARACTER INK1(NKEY1)*8, OUTK1(NKEY1)*32, CLASS*6, CDUMMY*1,
     *   CFIELD*11, CNAME*8, KEYW*8, TEST*32, ICLASS*6
      LOGICAL   OLDNAM
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INPUT.INC'
      INCLUDE 'INPUTDATA.INC'
C                                       Adverbs for IN
      DATA INK1 /'INNAME', 'INDISK', 'INSEQ', 'BLC', 'TRC'/
C                                       Rename
      DATA OUTK1 /'NAME', 'DISK', 'SEQ', 'BLC', 'TRC'/
C-----------------------------------------------------------------------
      NIMAGE = 0
      MAXDIM = 0
C                                       Startup
      CALL AV2INP (PRGN, NPARMS, AVNAME, AVTYPE, AVDIM, 'Input', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Declare 'ACTNOISE' a header
C                                       keyword for the image class.
      CNAME = 'IMAGE'
      KEYW = 'ACTNOISE'
      CALL OBVHKW (CNAME, KEYW, OOARE, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       other inputs
      CALL OGET ('Input', 'BADDISK', TYPE, DIM, IBAD, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'NFIELD', TYPE, DIM, NFIELD, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'NGAUSS', TYPE, DIM, NGAUSS, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      NMAPS = MAX (1, NFIELD) * MAX (1, NGAUSS)
      NMAPS = MAX (1, MIN (MAXFLD, NMAPS))
      CALL OGET ('Input', 'INCLASS', TYPE, DIM, DUMMY, CLASS, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       parms. defaults
      CALL OGET ('Input', 'APARM', TYPE, DIM, APARM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      EDGSKP = ABS (APARM(2))
      IF (EDGSKP.EQ.0) THEN
         EDGSKP = 5
         APARM(2) = EDGSKP
         END IF
      IF (APARM(1).LE.0.0) APARM(1) = 3.0
      CALL OPUT ('Input', 'APARM', TYPE, DIM, APARM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       allowed class types
      IF (CLASS.EQ.' ') CLASS = 'ICL001'
      OLDNAM = (CLASS(4:4).LT.'0') .OR. (CLASS(4:4).GT.'9') .OR.
     *   (CLASS(5:5).LT.'0') .OR. (CLASS(5:5).GT.'9') .OR.
     *   (CLASS(6:6).LT.'0') .OR. (CLASS(6:6).GT.'9')
      J0 = 1
      IF (.NOT.OLDNAM) READ (CLASS(4:6),1001) J0
      ICLASS = CLASS
      DO 20 KFIELD = 1,NMAPS
         LFIELD = KFIELD - 1 + J0
         WRITE (CFIELD,1000) LFIELD
         TEST = 'Input image object ' // CFIELD
         CALL CREATE (TEST, 'IMAGE', IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
         CALL IN2OBJ ('Input', NKEY1, INK1, OUTK1, TEST, IRET)
         IF (IRET.NE.0) GO TO 999
         DIM(1) = 6
         DIM(2) = 1
         IF (OLDNAM) THEN
            CALL ZEHEX (LFIELD-1, 2, CFIELD(:2))
            IF (LFIELD.GT.1) ICLASS(5:6) = CFIELD(:2)
         ELSE IF (LFIELD.LT.1000) THEN
            ICLASS(4:6) = CFIELD(2:4)
         ELSE
            ICLASS(3:6) = CFIELD(1:4)
            END IF
         CALL OPUT (TEST, 'CLASS', OOACAR, DIM, IDUMMY, ICLASS, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Open and close to check
         MSGSAV = MSGSUP
         MSGSUP = 32000
         CALL OOPEN (TEST, 'READ', IRET)
         MSGSUP = MSGSAV
C                                       skip missing ones
         IF (IRET.NE.0) THEN
            IRET = 0
C                                       got it - continue
         ELSE
            IF (NIMAGE.EQ.NMMAX) THEN
               MSGTXT = 'TOO MANY FIELDS*POINTINGS !!!!'
               CALL MSGWRT (8)
               IRET = 8
               GO TO 999
               END IF
            NIMAGE = NIMAGE + 1
            IN(NIMAGE) = TEST
            FNUM(NIMAGE) = LFIELD
C                                       accept header noise if available
            IF (APARM(5).LE.0.0) THEN
               MSGSUP = 32000
               CALL IMGET (TEST, 'ACTNOISE', TYPE, DIM, ACTN, CDUMMY,
     *            IRET)
               MSGSUP = MSGSAV
               IF ((IRET.NE.0) .OR. (ACTN.LE.0.0)) THEN
                  ACTN = 0.0
                  IRET = 0
                  END IF
C                                       force noise computation
            ELSE
               ACTN = 0.0
               END IF
            DIM(1) = 1
            DIM(2) = 1
            CALL OPUT (TEST, 'NOISE', OOARE, DIM, ACTN, CDUMMY, IRET)
            IF (IRET.NE.0) GO TO 999
C                                       Input subimage dimension
            CALL ARRWIN (TEST, BLC, TRC, NAXIS, IRET)
            IF (IRET.NE.0) GO TO 999
            NP = (TRC(1)-BLC(1)+1) * (TRC(2)-BLC(2)+1)
            MAXDIM = MAX (MAXDIM, NP)
            CALL COPY (5, BLC(3), TRC(3))
            DIM(1) = 7
            DIM(2) = 1
            CALL OPUT (TEST, 'BLC', OOAINT, DIM, BLC, CDUMMY, IRET)
            IF (IRET.NE.0) GO TO 999
            CALL OPUT (TEST, 'TRC', OOAINT, DIM, TRC, CDUMMY, IRET)
            IF (IRET.NE.0) GO TO 999
            CALL OCLOSE (TEST, IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
 20      CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I4.4)
 1001 FORMAT (I3)
      END
      SUBROUTINE SABOXD (IN, NIMAGE, FNUM, MAXDIM, NBOX, WIN, IRET)
C-----------------------------------------------------------------------
C   Loops over input fields: load memory, find rms, find islands,
C   write boxes.
C   Inputs:
C      IN       C(*)*?   Input image(s)
C      NIMAGE   I        Number images in IN
C      FNUM     I(*)     Facet number of each image
C      MAXDIM   I        Maximum image area in pixels
C   Output:
C      NBOX     I(*)     Number boxes written
C      IRET     I        Error code: 0 okay
C-----------------------------------------------------------------------
      CHARACTER IN(*)*(*)
      INTEGER   NIMAGE, FNUM(*), MAXDIM, NBOX(*), WIN(4,NIMAGE,*), IRET
C
      LONGINT   PIMAGE
      REAL      IMAGE(2), APARM(10), CXI, CXJ, CYI, CYJ, XI, XJ
      CHARACTER IBXFIL*48, OBXFIL*48, LINE*132
      INTEGER   TYPE, DIM(7), IDUMMY, ILUN, OLUN, IIND, OIND, J, I,
     *   NAXIS(7), NX, NY, BLC(7), TRC(7), MBOX, MSGSAV, LOV, NOV,
     *   FOV(5,2000), K, IB, JB, NGAUSS, NWORDS
      LOGICAL   CHANGED
      INCLUDE 'INCS:PCLN.INC'
      INTEGER   WINS(4,MXNBOX), IMSIZE(2,MAXFLD)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA ILUN, OLUN /10,11/
C-----------------------------------------------------------------------
      MBOX = MIN (MXNBOX, MXNBFL/NIMAGE)
      CHANGED = .FALSE.
C                                       allocate memory for work
      NWORDS = (MAXDIM - 1) / 1024 + 1
      CALL ZMEMRY ('GET ', 'SABOXD', NWORDS, IMAGE, PIMAGE, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'DYNAMIC MEMORY INADEQUATE: USE SUBIMAGES'
         GO TO 990
         END IF
C                                       box file names
      CALL OGET ('Input', 'NGAUSS', TYPE, DIM, NGAUSS, IBXFIL, IRET)
      IF (IRET.NE.0) GO TO 999
      NGAUSS = MAX (1, NGAUSS)
      CALL OGET ('Input', 'APARM', TYPE, DIM, APARM, IBXFIL, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'BOXFILE', TYPE, DIM, IDUMMY, IBXFIL, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'OBOXFILE', TYPE, DIM, IDUMMY, OBXFIL, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (OBXFIL.EQ.' ') OBXFIL = IBXFIL
      IF (OBXFIL.EQ.' ') THEN
         IRET = 8
         MSGTXT = 'OBOXFILE MUST BE SPECIFIED'
         GO TO 990
         END IF
C                                       open output text file
      CALL ZTXOPN ('WRIT', OLUN, OIND, OBXFIL, .TRUE., IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'OPEN OF OBOXFILE FAILS'
         GO TO 990
         END IF
C                                       copy input boxfile if diff
      IF ((IBXFIL.NE.' ') .AND. (IBXFIL.NE.OBXFIL)) THEN
         CALL ZTXOPN ('READ', ILUN, IIND, IBXFIL, .FALSE., IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'OPEN OF INPUT BOXFILE FAILS'
            GO TO 990
            END IF
C                                       read loop
 10      CALL ZTXIO ('READ', ILUN, IIND, LINE, IRET)
         IF (IRET.EQ.0) THEN
            CALL CHTRIM (LINE, 132, LINE, J)
C                                       copy non-box lines
            IF ((LINE(:1).LT.'0') .OR. (LINE(:1).GT.'9')) THEN
               CALL ZTXIO ('WRIT', OLUN, OIND, LINE(:J), IRET)
               IF (IRET.NE.0) THEN
                  MSGTXT = 'WRITE ERROR TO OBOXFILE'
                  GO TO 990
                  END IF
               END IF
            GO TO 10
         ELSE IF (IRET.NE.2) THEN
            MSGTXT = 'READ ERROR IN BOXFILE'
            GO TO 990
            END IF
C                                       end of file: close input
         CALL ZTXCLS (ILUN, IIND, IRET)
         END IF
C                                       loop through input images
      DO 100 I = 1,NIMAGE
         CALL OOPEN (IN(I), 'READ', IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) 'OPEN', FNUM(I)
            GO TO 990
            END IF
C                                       Input subimage dimension
         CALL ARRWIN (IN(I), BLC, TRC, NAXIS, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) 'WINDOW', FNUM(I)
            GO TO 990
            END IF
         NX = TRC(1) - BLC(1) + 1
         NY = TRC(2) - BLC(2) + 1
         IMSIZE(1,I) = NAXIS(1)
         IMSIZE(2,I) = NAXIS(2)
C                                       next level does real work
         J = 4 * MXNBOX
         CALL FILL (J, 0, WINS)
         CALL SABOXW (IN(I), FNUM(I), NX, NY, MBOX, IMAGE(1+PIMAGE),
     *      NBOX(I), WINS, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OCLOSE (IN(I), IRET)
C                                       copy boxes to output
         DO 20 J = 1,MAX(1,NBOX(I))
C                                       correct for blc
            IF (NBOX(I).GT.0) THEN
               IF (WINS(1,J).NE.-1) THEN
                  WINS(1,J) = WINS(1,J) + BLC(1) - 1
                  WINS(2,J) = WINS(2,J) + BLC(2) - 1
                  END IF
               WINS(3,J) = WINS(3,J) + BLC(1) - 1
               WINS(4,J) = WINS(4,J) + BLC(2) - 1
               END IF
            CALL COPY (4, WINS(1,J), WIN(1,FNUM(I),J))
 20         CONTINUE
         NBOX(I) = MAX (1, NBOX(I))
 100     CONTINUE
C                                       Beware of overlaps
      IF ((NIMAGE.GT.1) .AND. (APARM(8).LE.0.0)) THEN
         LOV = 2000
         MSGSAV = MSGSUP
 110     MSGSUP = 1
         IF (.NOT.CHANGED) MSGSUP = 0
         NOV = LOV
         CALL BOXCHK (NIMAGE, NGAUSS, IN, IMSIZE, NBOX, WIN, NOV, FOV,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
C                                       clean up overlapped windows
         IF (NOV.GT.0) THEN
            DO 140 K = 1,NOV
               IB = FOV(1,K)
               I  = FOV(2,K)
               JB = FOV(3,K)
               J  = FOV(4,K)
               IF ((WIN(1,I,IB).GT.-99) .AND. (WIN(1,J,JB).GT.-99) .AND.
     *            (FOV(5,K).GT.1)) THEN
                  IF (WIN(1,I,IB).LT.0) THEN
                     XI = PI * WIN(2,I,IB) * WIN(2,I,IB)
                     CXI = WIN(3,I,IB)
                     CYI = WIN(4,I,IB)
                  ELSE
                     XI = (WIN(3,I,IB) - WIN(1,I,IB) + 1.) *
     *                  (WIN(4,I,IB) - WIN(2,I,IB) + 1.)
                     CXI = (WIN(1,I,IB) + WIN(3,I,IB)) / 2.0
                     CYI = (WIN(2,I,IB) + WIN(4,I,IB)) / 2.0
                     END IF
                  IF (WIN(1,J,JB).LT.0) THEN
                     XJ = PI * WIN(2,J,JB) * WIN(2,J,JB)
                     CXJ = WIN(3,J,JB)
                     CYJ = WIN(4,J,JB)
                  ELSE
                     XJ = (WIN(3,J,JB) - WIN(1,J,JB) + 1.) *
     *                  (WIN(4,J,JB) - WIN(2,J,JB) + 1.)
                     CXJ = (WIN(1,J,JB) + WIN(3,J,JB)) / 2.0
                     CYJ = (WIN(2,J,JB) + WIN(4,J,JB)) / 2.0
                     END IF
                  IF (XJ.GT.1.1*XI) THEN
                     WIN(1,I,IB) = -999
                  ELSE IF (XI.GT.1.1*XJ) THEN
                     WIN(1,J,JB) = -999
                  ELSE
                     XI = (IMSIZE(1,I)/2.0-CXI)**2 +
     *                  (IMSIZE(2,I)/2.0-CYI)**2
                     XJ = (IMSIZE(1,J)/2.0-CXJ)**2 +
     *                  (IMSIZE(2,J)/2.0-CYJ)**2
                     IF (XJ.LT.0.90*XI) THEN
                        WIN(1,I,IB) = -999
                     ELSE IF (XI.LT.0.9*XJ) THEN
                        WIN(1,J,JB) = -999
                     ELSE IF (I.GT.J) THEN
                        WIN(1,I,IB) = -999
                     ELSE
                        WIN(1,J,JB) = -999
                        END IF
                     END IF
                  END IF
 140           CONTINUE
            DO 160 I = 1,NIMAGE
               JB = 0
               DO 150 IB = 1,NBOX(I)
                  IF (WIN(1,I,IB).GT.-99) THEN
                     JB = JB + 1
                     IF (JB.LT.IB) THEN
                        WIN(1,I,JB) = WIN(1,I,IB)
                        WIN(2,I,JB) = WIN(2,I,IB)
                        WIN(3,I,JB) = WIN(3,I,IB)
                        WIN(4,I,JB) = WIN(4,I,IB)
                        END IF
                     END IF
 150              CONTINUE
               WRITE (MSGTXT,1150) I, NBOX(I), JB
               IF (NBOX(I).NE.JB) THEN
                  CHANGED = .TRUE.
                  CALL MSGWRT (4)
                  END IF
               NBOX(I) = JB
 160           CONTINUE
            IF (NOV.EQ.LOV) GO TO 110
            END IF
C                                       repeat for 1-pt overlaps
         MSGSUP = 1
         NOV = LOV
         CALL BOXCHK (NIMAGE, NGAUSS, IN, IMSIZE, NBOX, WIN, NOV, FOV,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
C                                       report overlaps left
         DO 170 K = 1,NOV
            IB = FOV(1,K)
            I  = FOV(2,K)
            JB = FOV(3,K)
            J  = FOV(4,K)
            IF ((WIN(1,I,IB).GT.-99) .AND. (WIN(1,J,JB).GT.-99) .AND.
     *         (FOV(5,K).GE.1)) THEN
               WRITE (MSGTXT,1160) IB, I, JB, J, FOV(5,K)
               IF ((I.NE.J) .OR. (FOV(5,K).GT.1)) CALL MSGWRT (4)
               END IF
 170        CONTINUE
         END IF
C                                       write out windows
      DO 210 I = 1,NIMAGE
         IF (NBOX(I).LE.0) THEN
            CALL FILL (4, 0, WIN(1,FNUM(I),1))
            NBOX(I) = 1
            END IF
         DO 200 J = 1,NBOX(I)
            WRITE (LINE,1010) FNUM(I), WIN(1,FNUM(I),J),
     *         WIN(2,FNUM(I),J), WIN(3,FNUM(I),J), WIN(4,FNUM(I),J)
            CALL ZTXIO ('WRIT', OLUN, OIND, LINE(:40), IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) 'WRITE OBOXFILE', FNUM(I)
               GO TO 990
               END IF
 200        CONTINUE
 210     CONTINUE
C                                       close down
      CALL ZTXCLS (OLUN, OIND, IRET)
      CALL ZMEMRY ('FREE', 'SABOXD', NWORDS, IMAGE, PIMAGE, IRET)
      IRET = 0
      GO TO 999
C                                       error message printed
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR ON ',A,' IMAGE',I5)
 1010 FORMAT (I6.4,2X,4I8)
 1150 FORMAT ('BOXFIX: Field',I5,' number boxes reduced from',I5,' to',
     *   I5)
 1160 FORMAT ('Box',I5,' facet',I5,' overlaps box',I5,' facet',I5,' at',
     *   I2,' points')
      END
      SUBROUTINE SABOXW (IN, FNUM, NX, NY, MBOX, IMAGE, NBOX, WINS,
     *   IRET)
C-----------------------------------------------------------------------
C   SABOXW does the hard work: reads the image, finds the rms, finds
C   the islands, and fits the boxes
C   Inputs:
C      IN      C*(*)    Input image object already open
C      FNUM    I        Facet number
C      NX      I        Number X pixels
C      NY      I        Number Y pixels (rows)
C      MBOX    I        Max number boxes allowed
C   Outputs:
C      IMAGE   R(*,*)   Memory to use for image
C      NBOX    I        Number boxes found
C      WINS    I(4,*)   Boxes
C      IRET    I        Error code
C-----------------------------------------------------------------------
      CHARACTER IN*(*)
      INTEGER   FNUM, NX, NY, MBOX, NBOX, WINS(4,*), IRET
      REAL      IMAGE(NX,NY)
C
      INTEGER   MAXISL
      PARAMETER (MAXISL = 50000)
      INTEGER   IX, IY, DIM(7), TYPE, NPASS, NPK, PKWIN(4,MAXISL),
     *   NISLND, IPK, II, CX, CY, IROUND, JJ
      CHARACTER CDUMMY*1
      REAL      ACTN, SS, SQ, SN, T, RM, RS, RSP, RSM, X, Y, R,
     *   APARM(10)
      LOGICAL   DOMSG
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      NBOX = 0
      II = 4 * MAXISL
      CALL FILL (II, 0, PKWIN)
      DOMSG = .TRUE.
C                                       read the image
      DO 10 IY = 1,NY
         CALL ARREAD (IN, DIM, IMAGE(1,IY), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) 'READ', IY, FNUM
            GO TO 990
            END IF
 10      CONTINUE
      CALL ARRCLO (IN, IRET)
C                                       get rms
      CALL OGET (IN, 'NOISE', TYPE, DIM, ACTN, CDUMMY, IRET)
      IF (IRET.NE.0) THEN
         ACTN = 0.0
         IRET = 0
         END IF
C                                       get parameters
      CALL OGET ('Input', 'APARM', TYPE, DIM, APARM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       blank outside ellipse
      IF (APARM(2).GT.0.0) THEN
         R = (NX - 2.*APARM(2)) * (NY - 2.*APARM(2)) / 2.0
         R = R * R
         DO 20 IY = 1,NY
            Y = (IY - NY/2.0) * NX
            Y = Y * Y
            DO 15 IX = 1,NX
               X = (IX - NX/2.0) * NY
               X = X * X
               IF (X+Y.GT.R) IMAGE(IX,IY) = FBLANK
 15            CONTINUE
 20         CONTINUE
C                                       blank edgskip edge pixels
      ELSE
         JJ = -APARM(2) + 0.1
         DO 30 IY = 1,NY
            IF ((IY.LE.JJ) .OR. (IY.GT.NY-JJ)) THEN
               CALL RCOPY (NX, FBLANK, IMAGE(1,IY))
            ELSE
               CALL RCOPY (JJ, FBLANK, IMAGE(1,IY))
               CALL RCOPY (JJ, FBLANK, IMAGE(NX+1-JJ,IY))
               END IF
 30         CONTINUE
         END IF
C                                       we need to find it
      IF (ACTN.LE.0.0) THEN
         RSP = 1.E10
         RSM = -1.E10
         DO 60 NPASS = 1,10
            SS = 0.0
            SQ = 0.0
            SN = 0.0
            DO 50 IY = 1,NY
               DO 40 IX = 1,NX
                  T = IMAGE(IX,IY)
                  IF (T.NE.FBLANK) THEN
                     IF ((T.LT.RSP) .AND. (T.GT.RSM)) THEN
                        SS = SS + T
                        SQ = SQ + T * T
                        SN = SN + 1.0
                        END IF
                     END IF
 40               CONTINUE
 50            CONTINUE
            IF (SN.LE.0.0) THEN
               RSP = RSP + 3.0 * RS
               RSM = RSP - 3.0 * RS
            ELSE
               RM = SS / SN
               SQ = SQ / SN
               RS = SQ - RM * RM
               RS = SQRT (MAX (0.0, RS))
               RS = MAX (RS, 0.01*RM)
               RSP = RM + 3.0 * RS
               RSM = RM - 4.0 * RS
               END IF
 60         CONTINUE
         ACTN = RS
         END IF
      WRITE (MSGTXT,1060) FNUM, ACTN
      CALL MSGWRT (3)
C                                       find islands a la SAD
      ACTN = ACTN * APARM(1)
      NISLND = MAXISL
      IF (APARM(6).GT.0) NISLND = -NISLND
      CALL ISLAND (NISLND, NX, NY, IMAGE, ACTN, PKWIN, NPK)
C                                       how to turn into boxes??
      II = IROUND (APARM(3))
      JJ = IROUND (APARM(7))
      IF (JJ.LE.0) JJ = 2
      DO 100 IPK = 1,NPK
         IF ((PKWIN(1,IPK).LE.0) .OR. (PKWIN(2,IPK).LE.0) .OR.
     *      (PKWIN(3,IPK).GT.NX) .OR. (PKWIN(3,IPK).LT.PKWIN(1,IPK))
     *      .OR. (PKWIN(4,IPK).LT.PKWIN(2,IPK)) .OR.
     *      (PKWIN(4,IPK).GT.NY)) GO TO 100
         IX = PKWIN(3,IPK) - PKWIN(1,IPK) + 1
         IY = PKWIN(4,IPK) - PKWIN(2,IPK) + 1
         CX = (PKWIN(3,IPK) + PKWIN(1,IPK)) / 2
         CY = (PKWIN(4,IPK) + PKWIN(2,IPK)) / 2
C                                       drop single points unless strong
         IF (((IX.LT.JJ) .OR. (IY.LT.JJ)) .AND.
     *      (IMAGE(CX,CY).LT.3.*ACTN)) GO TO 100
C                                       circle
         IF (NBOX.LT.MBOX) THEN
            NBOX = NBOX + 1
            IF (((APARM(4).EQ.0.0) .AND. (ABS(IX-IY).LE.1) .AND.
     *         (MIN(IX,IY).LE.8)) .OR. (APARM(4).GT.0.0)) THEN
               IX = (MAX (IX,IY) + 1) / 2 + II
               WINS(1,NBOX) = -1
               WINS(2,NBOX) = IX
               WINS(3,NBOX) = CX
               WINS(4,NBOX) = CY
C                                       rectangle
            ELSE
               WINS(1,NBOX) = MAX (1, PKWIN(1,IPK) - II)
               WINS(2,NBOX) = MAX (1, PKWIN(2,IPK) - II)
               WINS(3,NBOX) = MIN (NX, PKWIN(3,IPK) + II)
               WINS(4,NBOX) = MIN (NY, PKWIN(4,IPK) + II)
               END IF
         ELSE
            MSGTXT = 'TOO MANY WINDOWS FOR ALLOWED BOX LIST'
            IF (DOMSG) CALL MSGWRT (7)
            DOMSG = .FALSE.
            END IF
 100     CONTINUE
      WRITE (MSGTXT,1100) FNUM, NBOX, NPK
      CALL MSGWRT (4)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SABOXW: ERROR DOING ',A,' AT ROW',I7,' FACET',I7)
 1030 FORMAT ('SABOXW: FACET',I5,' NO VALID PIXELS')
 1060 FORMAT ('Facet',I5,' using RMS',1PE12.4)
 1100 FORMAT ('Facet',I5,' found ',I5,' boxes in',I5,' islands')
      END
