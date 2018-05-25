C previous pixel's SIGMA, MAXPOS are used for the following pixel at row
C The option does work and tested, including using MEAN from ROBUST
C output as MAXPOS. Non linear Least Square:
C The maximum iteration is 4;
C relative change of SIGMA is changed from 10% to 1%;
C Range of the data for histogram: +-1.5sigma
C Works with the Frazer data; DLESQR with double precision is attached
C Condition to zero the Least Square Solution:
C (SIGMA.LE.0 .OR. (MAXIMU.LE.2.OR.MAXIMU.GT.1E6))
LOCAL INCLUDE 'XMBUFRS'
      INCLUDE 'INCS:PMAD.INC'
      REAL     BUFF1(MABFSS), BUFF2(MABFSS)
      COMMON /BUFRS/ BUFF1, BUFF2
LOCAL END
LOCAL INCLUDE 'RMSD.INC'
      INCLUDE 'XMBUFRS'
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6, OPCODE*4,
     *   OPTYPE*4
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAMOU(3), XCLAOT(2), XOPCOD,
     *   XOPTYP, CATOH(256)
      REAL      XSEQIN, XDISKI, XSEQO, XDISKO, BLC(7), TRC(7),
     *   BOX(2), FCUT, ACUT, RMSCUT, FLUXX, XINC, YINC
      INTEGER IMSIZE, IMSTRI, IMSBOX, IXINC, IYINC
C
C                                       MAXIMG = 32768(from PMAD.INC)
      PARAMETER (IMSIZE = MAXIMG/4)
      PARAMETER (IMSBOX = (IMSIZE/32)*(IMSIZE/32))
      PARAMETER (IMSTRI = (IMSIZE)*(IMSIZE/32))
      REAL  RMSAR(IMSTRI)
      REAL      CATOR(256)
      DOUBLE PRECISION CATOD(128)
      INTEGER   CATOLD(256), SEQIN, SEQOUT, DISKIN, DISKO, NEWCNO,
     *   OLDCNO, JBUFSZ
      INTEGER    LIM22, LIMYB, LIMYT, LMXL(IMSIZE), LMXR(IMSIZE),
     *   SGACUT, HBOX1, HBOX2
      LOGICAL  DOCIRC
      COMMON /INPARM/ XNAMEI, XCLAIN, XSEQIN, XDISKI,
     *   XNAMOU, XCLAOT, XSEQO, XDISKO, BLC, TRC, BOX, XOPCOD,
     *   XOPTYP, FCUT, ACUT, RMSCUT, FLUXX, XINC, YINC
      COMMON /CHPARM/ NAMEIN, CLAIN, NAMOUT, CLAOUT, OPCODE, OPTYPE
      COMMON /PARMS/  CATOLD, SEQIN, SEQOUT, DISKIN, DISKO, NEWCNO,
     *   OLDCNO, JBUFSZ, RMSAR, LIM22, LIMYB, LIMYT, LMXL, LMXR,
     *   SGACUT, IXINC, IYINC, HBOX1, HBOX2, DOCIRC
      EQUIVALENCE (CATOLD, CATOR, CATOH, CATOD)
LOCAL END
      PROGRAM RMSD
C-----------------------------------------------------------------------
C! RMSD creates map of RMS calculated for each pixel at the map plane.
C# Map-util
C-----------------------------------------------------------------------
C;  Copyright (C) 2002, 2005, 2007, 2008, 2010, 2014-2016
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
C   RMSD creates map of RMS (OPCODE=RMS) or MEAN (OPCODE=MEAN)
C   calculated for each pixel using the data inside of the given
C   box surrounded the pixel
C   If OPCODE=BLAN and the image value is less than RMS*ICUT then
C   RMSD subtitudes the image value by BLANK.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input image.
C      INCLASS        CLAIN         Class of input image.
C      INSEQ          SEQIN         Seq. of input image.
C      INDISK         DISKIN        Disk number of input image.
C      OUTNAME        NAMOUT        Name of the output image
C      OUTCLASS       CLAOUT        Class of output image.
C                                   Default output is input image.
C      OUTSEQ         SEQOUT        Seq. number of output image.
C      OUTDISK        DISKO         Disk number of the output image.
C      BLC(7)         BLC           Bottom left corner of subimage
C                                   of input image.
C      TRC(7)         TRC           Top right corner of subimage.
C      IMSIZE(2)      BOX           Size of the box
C      OPCODE         OPCODE        'RMS ','MEAN','BLAN','MAD','MEDI'
C      OPTYPE         OPTYPE        'ROBS', 'HIST'
C      SCALR1         FCUT          Number of RMS used at the cutoff
C      SCALR2         ACUT          Number of RMS used at the cutoff
C      SCALR3         RMSCUT        Number of RMS used at each iteration
C                                   calculating final RMS
C      XINC           XINC          Increment at X, pixels
C      YINC           YINC          Increment at Y, pixels
C   Programmer Leonia Kogan
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'RMSD.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'RMSD  '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL RMSDIN (PRGM, IRET)
C                                       Call routine that calculares
C                                       rms line and record it to the
C                                       output file
      IF (IRET.EQ.0) CALL RMSDDO (IRET)
C                                       History
      IF (IRET.EQ.0) CALL RMSHIS
C                                       Close down files, etc.
      CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE RMSDIN (PRGN, IRET)
C-----------------------------------------------------------------------
C   RMSDIN gets input parameters for RMSDS.
C   Inputs:  PRGN    C*6       Program name (2 chars/word)
C   Output:  IRET    I         Error code: 0 => ok
C                                4 => user routine detected error.
C                                5 => catalog troubles
C                                8 => can't start
C            /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      INTEGER  IRET
      CHARACTER PRGN*6
      CHARACTER   STAT*4, MTYPE*2
      INTEGER  IERR, NPARM, IROUND
      INCLUDE 'RMSD.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      JBUFSZ = 2 * MABFSS
      IRET = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
C                                       Get input parameters.
      NPARM = 38
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, BUFF1, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       Crunch input parameters.
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOT, CLAOUT)
      CALL H2CHR (4, 1, XOPCOD, OPCODE)
      CALL H2CHR (4, 1, XOPTYP, OPTYPE)
C                                       default of OPCODE is RMS
      IF ((OPCODE.NE.'MEAN') .AND. (OPCODE.NE.'BLAN') .AND.
     *   (OPCODE.NE.'MAD ') .AND. (OPCODE.NE.'MEDI')) OPCODE = 'RMS'
C                                       default of OPTYPE is 'ROBS'
      IF ((OPTYPE.NE.'ROBS') .AND. (OPTYPE.NE.'HIST')) OPTYPE = 'ROBS'
C                                       Sign of ACUT
      SGACUT = 1
      IF (ACUT.LT.-0.0001) SGACUT = -1
C                                       If FLUXX=0, no blanking
      IF (ABS(FLUXX).LT.1.0E-8) FLUXX = 1.0E8
C
C                                       default for RMSCUT(SCALR3)
      IF (RMSCUT.EQ.0.0) RMSCUT = 3.0
C
      SEQIN = IROUND (XSEQIN)
      SEQOUT = IROUND (XSEQO)
      DISKIN = IROUND (XDISKI)
      DISKO = IROUND (XDISKO)
      IXINC = IROUND (XINC)
      IF (IXINC.LE.0) IXINC = 1
      IYINC = IROUND (YINC)
      IF (IYINC.LE.0) IYINC = 1
      DOCIRC = BOX(2).LT.0.0
      IF (BOX(2).LE.0.) BOX(2) = BOX(1)
      BOX(1) = MAX (3., BOX(1))
      BOX(2) = MAX (3., BOX(2))
C                                       Half box
      HBOX1 = BOX(1) / 2
      HBOX2 = BOX(2) / 2
C                                       Do not allow
      IF (OPCODE.EQ.'BLAN') THEN
         IF (IYINC.GT.HBOX2) THEN
            IRET = 1
            WRITE (MSGTXT,1020)
            GO TO 990
            END IF
         END IF
C                                       Get CATBLK from old file.
      OLDCNO = 1
      MTYPE = 'MA'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, MTYPE,
     *   NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
C                                       Read CATBLK and mark 'READ'.
      CALL CATIO ('READ', DISKIN, OLDCNO, CATOLD, 'READ', BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
C
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
C                                       Copy old CATBLK to new.
      CALL COPY (256, CATOLD, CATBLK)
C                                       Put new values in CATBLK.
      CALL MAKOUT (NAMEIN, CLAIN, SEQIN, '      ', NAMOUT, CLAOUT,
     *   SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQOUT
C                                       Set defaults on BLC,TRC
      CALL WINDOW (CATOLD(KIDIM), CATOLD(KINAX), BLC, TRC, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get user modification to CATBLK
      CALL SUBHDR (BLC, TRC, 1, 1)
      IRET = 4
      NEWCNO = 1
      CALL MCREAT (DISKO, NEWCNO, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1075) IERR
         GO TO 990
         END IF
C                                       Record the creation
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = NEWCNO
      FRW(NCFILE) = 2
      IRET = 0
      SEQOUT = CATBLK(KIIMS)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RMSDIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1020 FORMAT ('You selected YINC>IMSIZEY; It is not allowed for ',
     *   'OPCODE=''BLAN'' ')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,' DISK=',
     *   I2,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1075 FORMAT ('ERROR',I5,' CREATING OUTPUT FILE')
      END
      SUBROUTINE RMSDDO (IRET)
C-----------------------------------------------------------------------
C   RMSDDO sends strip of the image (height of the strip is equal to
C   the height of the box) to the user supplied routine that calculates
C   rms based on the data at the box surrounding the point at the row
C   and then RMSDDO writes the modified row to the output map.
C   Output:
C      IRET   I  Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER IFILE*48
      INTEGER   LUNI, LUNO, NYI, NXI, WINI(4), NXO, NYO, WINO(4), BOI,
     *   BOO, LIM2, LIM3, LIM4, LIM5, LIM6, LIM7, I1, I2, I3, I4, I5,
     *   I6, I7, IPOS(7), CORN(7), BOTEMP, LIMO, LIMIT, IBIND, OBIND,
     *   INDI, INDO, LIM1, OUTCNT, LX, RX, IY, INDEX, NSTRIP, NPBOX, I20
      REAL      OUTMAX, OUTMIN
      LOGICAL   T, F, BLNKD
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'RMSD.INC'
      INCLUDE 'INCS:DCAT.INC'
      INTEGER   IX, IIY, KIY, IYOLD, IYNEW, INCRY, IND(IMSIZE), IMY,
     *   NMSG
      REAL      RMSOLD(IMSIZE), RMSNEW(IMSIZE), DELRMS(IMSIZE),
     *   RMSC(IMSIZE), IMVAL(IMSIZE), IMVALC, RMS
      DATA LUNI, LUNO /16,17/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Open the data files
      CALL ZPHFIL ('MA', DISKIN, OLDCNO, 1, IFILE, IRET)
      CALL ZOPEN (LUNI, INDI, DISKIN, IFILE, T, F, T, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
      CALL ZPHFIL ('MA', DISKO, NEWCNO, 1, IFILE, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, IFILE, T, T, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 990
         END IF
C                                       Setup for I/O
      NXI = CATOLD(KINAX)
      NYI = CATOLD(KINAX+1)
      NXO = CATBLK(KINAX)
      NYO = CATBLK(KINAX+1)
C                                       strip length
      LX = BLC(1)
      RX = TRC(1)
      WINI(1) = LX
      WINI(3) = RX
      NMSG = 0
C                                       The range of the second axes
C                                       (WINI(2,4)) depends on Y.
C                                       So WINI(2,4) will be determined
C                                       later
      WINO(1) = 1
      WINO(2) = 1
      WINO(3) = NXO
      WINO(4) = NYO
C
      OUTMAX = -1.0E20
      OUTMIN = 1.0E20
      BLNKD = F
C                                       Setup for looping
      LIM1 = TRC(1) - BLC(1) + 1.01
      LIM2 = TRC(2) - BLC(2) + 1.01
      LIM3 = TRC(3) - BLC(3) + 1.01
      LIM4 = TRC(4) - BLC(4) + 1.01
      LIM5 = TRC(5) - BLC(5) + 1.01
      LIM6 = TRC(6) - BLC(6) + 1.01
      LIM7 = TRC(7) - BLC(7) + 1.01
      CORN(7) = 1
C                                       The map's strip is too big
      NSTRIP = LIM1*BOX(2)
      IF (NSTRIP.GT.IMSTRI) THEN
         IRET = 1
         WRITE (MSGTXT,1030)
         GO TO 990
         END IF
C                                       The box is too big
      NPBOX = BOX(1)*BOX(2)
      IF (NPBOX.GT.IMSBOX) THEN
         IRET = 1
         WRITE (MSGTXT,1035)
         GO TO 990
         END IF
C                                       The X size is too big
      IF (LIM1.GT.IMSIZE) THEN
         IRET = 1
         WRITE (MSGTXT,1040) IMSIZE
         GO TO 990
         END IF
C                                       array of left and right edges
C                                       of the box along X
      DO 20 I1 = 1, LIM1
         LMXL(I1) = MAX((I1 - HBOX1), 1)
         LMXR(I1) = MIN((I1 + HBOX1 - 1), LIM1)
 20      CONTINUE
C
      LIMO = CATBLK(KINAX) - 1
C                                       Loop
      DO 700 I7 = 1,LIM7
         IPOS(7) = BLC(7) + I7 - 0.9
         CORN(7) = I7
         DO 600 I6 = 1,LIM6
            IPOS(6) = BLC(6) + I6 - 0.9
            CORN(6) = I6
            DO 500 I5 = 1,LIM5
               IPOS(5) = BLC(5) + I5 - 0.9
               CORN(5) = I5
               DO 400 I4 = 1,LIM4
                  IPOS(4) = BLC(4) + I4 - 0.9
                  CORN(4) = I4
                  DO 300 I3 = 1,LIM3
                     IPOS(3) = BLC(3) + I3 - 0.9
                     CORN(3) = I3
                     WRITE (MSGTXT,1050) IPOS(3)
                     CALL MSGWRT (1)
C                                       Init. files, first input.
         CALL COMOFF (CATOLD(KIDIM), CATOLD(KINAX), IPOS(3), BOTEMP,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1060) IRET
            GO TO 990
            END IF
         BOI = BOTEMP + 1
C                                       Init output file.
         CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), CORN(3), BOTEMP,
     *      IRET)
         BOO = BOTEMP + 1
         CALL MINIT ('WRIT', LUNO, INDO, NXO, NYO, WINO, BUFF2,
     *      JBUFSZ, BOO, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) 'WRIT', IRET
            GO TO 990
            END IF
         OUTCNT = NYO
C                                       cycle by IY reading
C                                       strip of the data along X
C                                       near IY
         IIY = 0
         IMY = 0
C                                       IY=1,IY=IY+IYINC...LIM2
C                                       The last point should be LIM2
C                                       for any IYINC
         IY = 1
 100     CONTINUE
            LIMYB = MAX((IY - HBOX2), 1)
            LIMYT = MIN((IY + HBOX2 - 1), LIM2)
C
            I20 = IY - LIMYB + 1
            LIM22 = LIMYT - LIMYB + 1
C                                       window along the Y axis depends
C                                       on IY
            WINI(2) = LIMYB + BLC(2) - 1
            WINI(4) = LIMYT + BLC(2) - 1
C                                       initiate to read the given strip
            CALL MINIT ('READ', LUNI, INDI, NXI, NYI, WINI, BUFF1,
     *         JBUFSZ,BOI, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1100) 'READ', IRET
               GO TO 990
               END IF
            INDEX = 1
            DO 120 I2 = 1,LIM22
C                                       Read.
               CALL MDISK ('READ', LUNI, INDI, BUFF1, IBIND, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1120) 'READ', IRET
                  GO TO 990
                  END IF
C                                       store the strip in RMSAR(INDEX)
               CALL RCOPY (LIM1, BUFF1(IBIND), RMSAR(INDEX))
               INDEX = INDEX + LIM1
  120          CONTINUE
C
            IF (IY.EQ.1) THEN
               CALL MDISK ('WRIT', LUNO, INDO, BUFF2, OBIND, IRET)
               OUTCNT = OUTCNT - 1
               IF (OUTCNT.LT.0) THEN
                  WRITE (MSGTXT,1140)
                  GO TO 990
                  END IF
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1120) 'WRIT', IRET
                  GO TO 990
                  END IF
               END IF
C                                       the RMSBOX calculates the row
C                                       of rmses and means based on the
C                                       data at the strip surrounding the
C                                       row. The strip RMSAR is used as
C                                       the input data of the required
C                                       height.
            IF ((OPCODE.EQ.'MAD') .OR. (OPCODE.EQ.'MEDI')) THEN
               CALL MADBOX (LIM1, RMSNEW, IRET)
            ELSE
               CALL RMSBOX (LIM1, RMSNEW, NMSG, IRET)
               END IF
C                                       calculate the image value
C                                       at the given node IY
            IF (OPCODE.EQ.'BLAN') THEN
               DO 130 IX = 1,LIM1
                  INDEX = IX + (I20-1)*LIM1
                  IND(IX) = INDEX
                  IF (IY.EQ.1) IMVAL(IX) = RMSAR(INDEX)
  130             CONTINUE
               END IF
C                                       start interpolation
C                                       store RMS line at RMSNEW for
C                                       the following interpolation
            IF (IY.EQ.1) THEN
               IIY = IIY + 1
C                                       interpolation
            ELSE
               IYNEW = IY
               INCRY = IYNEW - IYOLD
               DO 140 IX = 1,LIM1
                  DELRMS(IX) = (RMSNEW(IX)-RMSOLD(IX)) / INCRY
                  RMSC(IX) = RMSOLD(IX)
                  IF (OPCODE.EQ.'BLAN') IND(IX) = IND(IX) - LIM1*INCRY
 140              CONTINUE
               DO 180 KIY = 1,INCRY
C                                       squirrel away first row
                  IF ((IIY.EQ.1) .AND. (KIY.EQ.1)) THEN
                     DO 150 IX = 1,LIM1
                        RMS = RMSOLD(IX)
                        IF (OPCODE.EQ.'BLAN') THEN
                           IMVALC = IMVAL(IX)
                           IF ((IMVALC.GT.FCUT*RMS) .AND.
     *                        (SGACUT*ABS(IMVALC).GT.ACUT*RMS) .AND.
     *                        (RMS.LT.FLUXX)) THEN
                              BUFF2(OBIND+IX-1) = IMVALC
                           ELSE
                              BUFF2(OBIND+IX-1) = FBLANK
                              END IF
                        ELSE
                           BUFF2(OBIND+IX-1) = RMS
                           END IF
 150                    CONTINUE
                     END IF
C                                       record the interpolated line
                  IIY = IIY + 1
                  CALL MDISK ('WRIT', LUNO, INDO, BUFF2, OBIND, IRET)
                  OUTCNT = OUTCNT - 1
                  IF (OUTCNT.LT.0) THEN
                     MSGTXT = 'RMSDDO: OUTCNT.LT.0! TOO MANY OUTPUT' //
     *                  ' ROWS RETURNED'
                     GO TO 990
                     END IF
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1120) 'WRIT', IRET
                     GO TO 990
                     END IF
C
                  DO 170 IX = 1,LIM1
                     RMSC(IX) = RMSC(IX) + DELRMS(IX)
                     RMS = RMSC(IX)
                     IF (OPCODE.EQ.'BLAN') THEN
C                                       calculate the current line
C                                       of the image
                        IND(IX) = IND(IX) + LIM1
                        INDEX = IND(IX)
                        IMVALC = RMSAR(INDEX)
                        IF ((IMVALC.GT.FCUT*RMS) .AND.
     *                     (SGACUT*ABS(IMVALC).GT.ACUT*RMS) .AND.
     *                     (RMS.LT.FLUXX)) THEN
                           BUFF2(OBIND+IX-1) = IMVALC
                        ELSE
                           BUFF2(OBIND+IX-1) = FBLANK
                           END IF
                     ELSE
                        BUFF2(OBIND+IX-1) = RMS
                        END IF
 170                 CONTINUE
 180              CONTINUE
               END IF
C                                       Check max, min, blanking.
            LIMIT = OBIND + LIMO
            DO 200 I1 = OBIND,LIMIT
               IF (BUFF2(I1).NE.FBLANK) THEN
                  OUTMAX = MAX (OUTMAX, BUFF2(I1))
                  OUTMIN = MIN (OUTMIN, BUFF2(I1))
               ELSE
                  BLNKD = .TRUE.
                  END IF
 200           CONTINUE
C                                       store RMS and lines for
C                                       the following interpolation
            DO 260 IX = 1, LIM1
               RMSOLD(IX) = RMSNEW(IX)
 260           CONTINUE
            IYOLD = IY
C                                       modify IY to take the last
C                                       point LIM2
            IY = IY + IYINC
            IF (IY.LE.LIM2) THEN
               GO TO 100
            ELSE IF ((IY-LIM2).LT.IYINC) THEN
               IY = LIM2
               GO TO 100
               END IF
C                                       end of the Y cycle
C
C                                       Flush buffer.
            CALL MDISK ('FINI', LUNO, INDO, BUFF2, OBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1120) 'FINI', IRET
               GO TO 990
               END IF
C                                       Update CATBLK.
            CATR(KRDMX) = OUTMAX
            CATR(KRDMN) = OUTMIN
            CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'REST', BUFF1,
     *         IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1180) IRET
               GO TO 990
               END IF
 300     CONTINUE
 400              CONTINUE
 500           CONTINUE
 600        CONTINUE
 700     CONTINUE
C                                       Mark blanking in CATBLK.
      CATR(KRBLK) = 0.0
      IF (BLNKD) CATR(KRBLK) = FBLANK
C                                       Close input map.
      CALL ZCLOSE (LUNI, INDI, IRET)
      CALL ZCLOSE (LUNO, INDO, IRET)
      IRET = 0
      IF (NMSG.GT.1) THEN
         WRITE (MSGTXT,1700) NMSG
         CALL MSGWRT (6)
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RMSDDO: ERROR',I3,' OPENING INPUT FILE')
 1020 FORMAT ('RMSDDO: ERROR',I5,' OPENING OUTPUT FILE')
 1030 FORMAT ('STRIP is too big. Change BLC,TRC(1,2) and/or IMSIZE')
 1035 FORMAT ('BOX is too big. Change IMSIZE')
 1040 FORMAT ('X size exceeds limit= ', I5,
     *   '. Change BLC(1), TRC(1)')
 1050 FORMAT ('Beginning plane',I4)
 1060 FORMAT ('RMSDDO: COMOFF ERROR',I3)
 1100 FORMAT ('RMSDDO: INIT-FOR-',A4,' ERROR',I3)
 1120 FORMAT ('RMSDDO: ',A,' ERROR',I3)
 1140 FORMAT ('RMSDDO: OUTCNT.LT.0! Too many output rows returned.')
 1180 FORMAT ('RMSDDO: CATIO ERROR',I3,' UPDATING CATBLK')
 1700 FORMAT ('Matrix inconsistency message repeated',i7,' times')
      END
      SUBROUTINE RMSHIS
C-----------------------------------------------------------------------
C   RMSHIS copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER LINE*80, NOTTYP*2
      INTEGER   LUN1, LUN2, IERR, XWIN, YWIN, IROUND
      LOGICAL   T
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'RMSD.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
      DATA NOTTYP /'CC'/
C-----------------------------------------------------------------------
C                                       copy some keywords
      CALL KEYPCP (DISKIN, OLDCNO, DISKO, NEWCNO, 0, ' ', IERR)
      XWIN = IROUND(BOX(1))
      YWIN = IROUND(BOX(2))
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, OLDCNO, NEWCNO, CATBLK,
     *   BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 200
         END IF
C                                       New history
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 200
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2,
     *   BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       BLC
      WRITE (LINE,2000) TSKNAM, BLC
      CALL HIADD (LUN2, LINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       TRC
      WRITE (LINE,2001) TSKNAM, TRC
      CALL HIADD (LUN2, LINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
      IF (DOCIRC) THEN
C                                       the circle diameter
         WRITE (LINE,2008) TSKNAM, XWIN
         CALL HIADD (LUN2, LINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
      ELSE
C                                       X window
         WRITE (LINE,2002) TSKNAM, 'X', XWIN, 'columns'
         CALL HIADD (LUN2, LINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
C                                       Y window
         WRITE (LINE,2002) TSKNAM, 'Y', YWIN, 'rows'
         CALL HIADD (LUN2, LINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       OPCODE
      WRITE (LINE,2003) TSKNAM, OPCODE
      CALL HIADD (LUN2, LINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       OPTYPE
      WRITE (LINE,2006) TSKNAM, OPTYPE
      CALL HIADD (LUN2, LINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       SCALR1, SCALR2, SCALR3
      IF (OPTYPE.EQ.'BLAN') THEN
         WRITE (LINE,2004) TSKNAM, FCUT, ACUT, RMSCUT
         CALL HIADD (LUN2, LINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
C                                       FLUX
         IF (FLUXX.GT.1.E7) FLUXX = 0
         WRITE (LINE,2007) TSKNAM, FLUXX
         CALL HIADD (LUN2, LINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       XINC, YINC
      WRITE (LINE,2005) TSKNAM, XINC, YINC
      CALL HIADD (LUN2, LINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Close HI file
 200  CALL HICLOS (LUN2, T, BUFF2, IERR)
C                                        Copy tables
      CALL ALLTAB (1, NOTTYP, LUN1, LUN2, DISKIN, DISKO, OLDCNO,
     *   NEWCNO, CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         MSGTXT = 'ERROR COPYING TABLE FILES'
         CALL MSGWRT (6)
         END IF
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'REST', BUFF1, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RMSHIS: ERROR',I3,' COPY/OPEN HISTORY FILE')
 2000 FORMAT (A6,'BLC =',6(F6.0,','),F6.0)
 2001 FORMAT (A6,'TRC =',6(F6.0,','),F6.0)
 2002 FORMAT (A6,A1,'WIN = ',I3,10X,' / ',A,' in the box')
 2003 FORMAT (A6,'OPCODE = ', A4)
 2004 FORMAT (A6,'SCALR1 = ', F6.3, ' SCALR2 = ', F6.3,
     *   ' SCALR3 = ', F6.3)
 2005 FORMAT (A6,'XINC = ', F6.0, ' YINC = ', F6.0)
 2006 FORMAT (A6,'OPTYPE = ', A4)
 2007 FORMAT (A6,'FLUX = ', F6.2)
 2008 FORMAT (A6,'The circular box with diameter', I3,
     *   ' pixels is used')
      END
      SUBROUTINE RMSBOX (LIM1, RMSS, NMSG, IRET)
C-----------------------------------------------------------------------
C   RMSBOX uses strip of the image around the given row to calculate
C   arrays of rms and  mean for each pixel at the row
C   Inputs:
C      LIM1     I      The strip length (X)
C   Inputs in common:
C      RMSAR    R(*)   Strip of the image surrounding the row
C      LIM22    I      The strip height (Y)
C      LMXL     I      Left edge of the box in the strip
C      LMXR     I      Right edge of the box in the strip
C      FBLANK   R      Value of blanked pixel.
C   Output:
C      RMSS     R(*)   Output row of rmses
C      IRET     I      Return code   0 => OK
C                         >0 => error, terminate.
C-----------------------------------------------------------------------
      INTEGER   LIM1, NMSG, IRET
      REAL      RMSS(*)
C
      INTEGER   IX, LIMXL, LIMXR, ICOUNT, NCOUNT, I1, I2, INDEX, IIX,
     *   IXOLD, INCRX, KIX
      REAL      RMSOLD, MEAOLD, DELRMS, DELMEA, SIGPRE, MAXPRE,
     *   MPRE, MEAN, RMS, XCENTE, YCENTE, XCUR, YCUR, RSQUAR, HBSQ
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'RMSD.INC'
      REAL     RMSIN(IMSBOX)
      LOGICAL  BLANK(IMSBOX)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       calculate
C                                       rms using the box around the
C                                       point IX
      IIX = 0
C                                       beginning of the cycle by IX
C                                       IX =1,IX=IX+IXINC...LIM1
C                                       The last point should be LIM1
C                                       for any IXINC
      IX = 1
      IXOLD = 1
      HBSQ = HBOX2 * HBOX2
C                                       Loop point - next IX center
 100  CONTINUE
         LIMXL = LMXL(IX)
         LIMXR = LMXR(IX)
         ICOUNT = 0
         DO 160 I2 = 1,LIM22
            INDEX = LIMXL - 1 + (I2-1)*LIM1
            DO 140 I1 = LIMXL,LIMXR
               INDEX = INDEX + 1
               IF (RMSAR(INDEX).NE.FBLANK) THEN
C                                       Drop out the square's
C                                       points located outside of
C                                       the relevant circle
                  IF (DOCIRC) THEN
                     XCENTE = (LIMXL + LIMXR) / 2.0
                     YCENTE = (LIM22 + 1.0) / 2.0
                     XCUR = I1
                     YCUR = I2
                     RSQUAR = (XCUR-XCENTE)*(XCUR-XCENTE) +
     *                  (YCUR-YCENTE)*(YCUR-YCENTE)
                     IF (RSQUAR.LE.HBSQ) THEN
                        ICOUNT = ICOUNT + 1
                        RMSIN(ICOUNT) = RMSAR(INDEX)
                        BLANK(ICOUNT) = .TRUE.
                        END IF
                  ELSE
                     ICOUNT = ICOUNT + 1
                     RMSIN(ICOUNT) = RMSAR(INDEX)
                     BLANK(ICOUNT) = .TRUE.
                     END IF
                  END IF
 140           CONTINUE
 160        CONTINUE
C
         NCOUNT = ICOUNT
C                                       OPTYPE = 'ROBS'/'HIST'
         IF (OPTYPE.EQ.'HIST') THEN
            CALL HISTOG (RMSIN, BLANK, IX, NCOUNT, RMS, MEAN,
     *         SIGPRE, MAXPRE, MPRE, NMSG, IRET)
         ELSE
            CALL ROBUST (RMSIN, BLANK, NCOUNT, RMS, MEAN)
            END IF
C                                       The current RMS is the RMSNEW
C                                       or no interpolation
         IF ((IX.EQ.1) .OR. (IXINC.EQ.1)) THEN
            IIX = IIX + 1
            IF (OPCODE.EQ.'MEAN') THEN
               RMSS(IIX) = MEAN
            ELSE
               RMSS(IIX) = RMS
               END IF
C                                       interpolation
         ELSE
            INCRX = IX - IXOLD
            IF (OPCODE.EQ.'MEAN') THEN
               DELMEA = (MEAN - RMSS(IIX)) / INCRX
               DO 240 KIX = 1,INCRX-1
                  IIX = IIX + 1
                  RMSS(IIX) = RMSS(IIX-1) + DELMEA
240               CONTINUE
               IIX = IIX + 1
               RMSS(IIX) = MEAN
            ELSE
               DELRMS = (RMS - RMSS(IIX)) / INCRX
               DO 241 KIX = 1,INCRX-1
                  IIX = IIX + 1
                  RMSS(IIX) = RMSS(IIX-1) + DELRMS
 241              CONTINUE
               IIX = IIX + 1
               RMSS(IIX) = RMS
               END IF
            END IF
C                                       store RMS and MEAN for the
C                                       following interpolation
         MEAOLD = MEAN
         RMSOLD = RMS
         IXOLD = IX
C                                       Modify IX to take the last point
C                                       LIM1
         IX = IX + IXINC
         IF (IX.LE.LIM1) THEN
            GO TO 100
         ELSE IF ((IX-LIM1).LT.IXINC) THEN
            IX = LIM1
            GO TO 100
            END IF
C
 999  RETURN
      END
      SUBROUTINE MADBOX (LIM1, RMSS, IRET)
C-----------------------------------------------------------------------
c   MADBOX uses strip of the image around the given row to calculate
C   arrays of median or median absolute deviation
C   Inputs:
C      LIM1     I      The strip length (X)
C   Inputs in common:
C      RMSAR    R(*)   Strip of the image surrounding the row
C      LIM22    I      The strip height (Y)
C      LMXL     I      Left edge of the box in the strip
C      LMXR     I      Right edge of the box in the strip
C      FBLANK   R      Value of blanked pixel.
C   Output:
C      RMSS     R(*)   Output row of rmses
C      IRET     I      Return code   0 => OK
C                         >0 => error, terminate.
C-----------------------------------------------------------------------
      INTEGER   LIM1, IRET
      REAL      RMSS(*)
C
      INTEGER   IX, LIMXL, LIMXR, ICOUNT, NCOUNT, I1, I2, INDEX, IIX,
     *   IXOLD, INCRX, KIX
      REAL      VALOLD, DELVAL, VAL, XCENTE, YCENTE, XCUR, YCUR, RSQUAR,
     *   HBSQ, MEDIAN, MEDV
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'RMSD.INC'
      REAL     RMSIN(IMSBOX)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
C----------------------------------------------------------------------
C                                       calculate
C                                       rms using the box around the
C                                       point IX
      IIX = 0
C                                       beginning of the cycle by IX
C                                       IX =1,IX=IX+IXINC...LIM1
C                                       The last point should be LIM1
C                                       for any IXINC
      IX = 1
      IXOLD = 1
      HBSQ = HBOX2 * HBOX2
C                                       Loop point - next IX center
 100  CONTINUE
         LIMXL = LMXL(IX)
         LIMXR = LMXR(IX)
         ICOUNT = 0
         DO 120 I2 = 1,LIM22
            INDEX = LIMXL - 1 + (I2-1)*LIM1
            DO 110 I1 = LIMXL,LIMXR
               INDEX = INDEX + 1
               IF (RMSAR(INDEX).NE.FBLANK) THEN
C                                       Drop out the square's
C                                       points located outside of
C                                       the relevant circle
                  IF (DOCIRC) THEN
                     XCENTE = (LIMXL + LIMXR) / 2.0
                     YCENTE = (LIM22 + 1.0) / 2.0
                     XCUR = I1
                     YCUR = I2
                     RSQUAR = (XCUR-XCENTE)*(XCUR-XCENTE) +
     *                  (YCUR-YCENTE)*(YCUR-YCENTE)
                     IF (RSQUAR.LE.HBSQ) THEN
                        ICOUNT = ICOUNT + 1
                        RMSIN(ICOUNT) = RMSAR(INDEX)
                        END IF
                  ELSE
                     ICOUNT = ICOUNT + 1
                     RMSIN(ICOUNT) = RMSAR(INDEX)
                     END IF
                  END IF
 110           CONTINUE
 120        CONTINUE
         NCOUNT = ICOUNT
         MEDV = MEDIAN (NCOUNT, RMSIN)
         IF (OPCODE.EQ.'MAD') THEN
            DO 130 ICOUNT = 1,NCOUNT
               RMSIN(ICOUNT) = ABS (RMSIN(ICOUNT) - MEDV)
 130           CONTINUE
            VAL = 1.4826 * MEDIAN (NCOUNT, RMSIN)
         ELSE
            VAL = MEDV
            END IF
C                                       The current RMS is the RMSNEW
C                                       or no interpolation
         IF ((IX.EQ.1) .OR. (IXINC.EQ.1)) THEN
            IIX = IIX + 1
            RMSS(IIX) = VAL
C                                       interpolation
         ELSE
            INCRX = IX - IXOLD
            DELVAL = (VAL - RMSS(IIX)) / INCRX
            DO 140 KIX = 1,INCRX-1
               IIX = IIX + 1
               RMSS(IIX) = RMSS(IIX-1) + DELVAL
 140           CONTINUE
            IIX = IIX + 1
            RMSS(IIX) = VAL
            END IF
C                                       store RMS and MEAN for the
C                                       following interpolation
         VALOLD = VAL
         IXOLD = IX
C                                       Modify IX to take the last point
C                                       LIM1
         IX = IX + IXINC
         IF (IX.LE.LIM1) THEN
            GO TO 100
         ELSE IF ((IX-LIM1).LT.IXINC) THEN
            IX = LIM1
            GO TO 100
            END IF
C
 999  RETURN
      END
      SUBROUTINE HISTOG (RMSIN, BLANK, IX, NCOUNT, SIGMA, MEAN, SIGPRE,
     *   MAXPRE, MPRE, NMSG, IRET)
C-----------------------------------------------------------------------
C   Given array of the input data RMSIN, the routine estimates
C   RMS(SIGMA) and MEAN using histogram concept
C
C   Routine create the histogram files XARG, FUNC and the fits
C   the three parameters of the one dimensional Gaussian
C   (MAXIMU, MAXPOS, SIGMA) using the array of argument XARG and
C   function FUNC.
C   Initial estimation is carried out using the subroutine ROBUST
C   The final fitting is carried out by several iterations of Least
C   Square procedure.
C   Input:
C      RMSIN   R(*)  Array of input data
C      NCOUNT  I     Number of point in RMSIN
C      IX      I     pixel number at the horizontal row
C   Output:
C      SIGMA   R     Solution for the rms
C      MEAN    R     Solution for MEAN
C      SIGPRE  R     Sigma for the following pixel
C      MAXPRE  R     Position of  histogram maximum for following pixel
C      MPRE    R     Value of maximum of histogram for following pixel
C      IRET    I     Error; 0 => OK
C-----------------------------------------------------------------------
      REAL     RMSIN(*), SIGMA, MEAN, SIGPRE, MAXPRE, MPRE
      LOGICAL  BLANK(*)
      INTEGER  IX, NCOUNT, NMSG, IRET
C
      INTEGER  NVALL, NFIT
      PARAMETER (NVALL=1000)
      REAL      VALL(NVALL), XARG, GAUSS, RIGHT, F1, F2, F3, MAXVAL,
     *   MINVAL, DVAL, COEFF, SIGOLD, SIGNEW, MAXIMU, MAXPOS, ALPHA,
     *   SIGROB, MPOROB, MAXROB
      INTEGER   NRANGE, IRANGE, ICOUNT, II, IMAX, ITER, I, IFIT, KFIT,
     *   IKFIT
      LOGICAL   BIGG, START, ROBPRE
      DOUBLE PRECISION FITPAR(3), R(3), MATR(9), NOBS, SUM, SSQ,
     *   VX(20), SSQRES, VARRES, VARY, FIT
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IF (NCOUNT.LE.0) THEN
         SIGMA = FBLANK
         MEAN = FBLANK
         GO TO 999
         END IF
    5 CONTINUE
C                                       return here if the least square
C                                       for histogramm failed
      START = (IX.EQ.1) .OR. (SIGMA.EQ.0)
      IF (START) THEN
C                                       find solution for SIGMA, MAXPOS
C                                       for the first pixel at the row
C                                       using the old RMSD technology
C
         CALL ROBUST (RMSIN, BLANK, NCOUNT, SIGMA, MEAN)
         IF ((SIGMA.EQ.0) .OR. (SIGMA.EQ.FBLANK)) GO TO 999
         MAXPOS = MEAN
C                                       store SIGMA, MEAN after ROBUST
C                                       and the fact of the ROBUST run
         SIGROB = SIGMA
         MPOROB = MEAN
         ROBPRE = .TRUE.
C
      ELSE
C                                       IX.NE.1=>Pick up SIGMA, MAXPOS
C                                       and MPRE from the previous pixel
C                                       at the row
         SIGMA = SIGPRE
         MAXPOS = MAXPRE
         MAXIMU = MPRE
C                                       ROBUST was not run
         ROBPRE = .FALSE.
         END IF
C                                       SIGMA and MAXPOS have been found
C                                       for both IX=1, and IX>1
C
C                                       determine MINVAL, MAXVAL
      COEFF = 1
      ALPHA = 1.5
C
      MINVAL = MAXPOS - ALPHA*COEFF*SIGMA
      MAXVAL = MAXPOS + ALPHA*COEFF*SIGMA
C                                       NRANGE is number of intervals
C                                       between MINVAL and MAXVAL
      NRANGE = 17 * COEFF
      DVAL = (MAXVAL - MINVAL) / (NRANGE - 1)
      MINVAL = MINVAL - DVAL/2.0
      MAXVAL = MAXVAL + DVAL/2.0
C                                       zero histogram
      DO 10 IRANGE = 1,NRANGE
         VALL(IRANGE) = 0.0
 10      CONTINUE
C                                       fill histogram
      DO 40 ICOUNT = 1,NCOUNT
         IRANGE = (RMSIN(ICOUNT) - MINVAL) / DVAL + 1.0
         IF ((IRANGE.GE.1) .AND. (IRANGE.LE.NRANGE))
     *      VALL(IRANGE) = VALL(IRANGE) + 1.0
 40      CONTINUE
C                                       find value of maximum for IX=1
      IF (START) THEN
         MAXIMU = 0.0
         DO 50 II = 1,NRANGE
            IF (VALL(II).GT.MAXIMU) THEN
               MAXIMU = VALL(II)
               IMAX = II
               END IF
   50       CONTINUE
C                                       store the initial value of
C                                       maximum
         MAXROB = MAXIMU
         END IF
C                                       Initial solution for MAXIMU
C                                       (IX=1) is found.
C                                       Initial solution (IX=1) for
C                                       SIGMA, MAXPOS was found as
C                                       output of ROBUST
C
C                                       IF (IX.GT.1) then the initial
C                                       solution for  SIGMA, MAXPOS, and
C                                       MAXIMU is found as least
C                                       square solution of the previous
C                                       pixel
C
C                                       store the first estimation of
C                                       SIGMA as SIGOLD
      SIGOLD = SIGMA
C                                       The following is non linear
C                                       least square to define more
C                                       precisely the three parameters
      ITER = 1
      BIGG = .TRUE.
      IRET = 0
C
      NFIT = 3
C                                       next iteraion of LEASQ
 100  IF ((ITER.LE.4) .AND. (BIGG)) THEN
C                                       Force result vector R(NFIT),
C                                       matrix M(NFIT*NFIT) to zero
         DO 120 IFIT = 1,NFIT
            R(IFIT) = 0.0D0
            DO 110 KFIT = 1,NFIT
               IKFIT = IFIT + (KFIT - 1)*NFIT
               MATR(IKFIT) = 0.0D0
 110           CONTINUE
 120        CONTINUE
         SUM = 0.0
         SSQ = 0.0
         NOBS = 0.0
C                                       Prepare result vector R(NFIT)
C                                       and matrix MATR(NFIT*NFIT)
C                                       for routine LEASQR
         XARG = MINVAL + DVAL/2.0
         DO 130 I = 1,NRANGE
C                                       The Gaussian's value with
C                                       initial parameters
            GAUSS = MAXIMU *
     *         EXP(-(XARG-MAXPOS)*(XARG-MAXPOS)/2.0/SIGMA/SIGMA)
C                                       The right part of the equations
            RIGHT = VALL(I) - GAUSS
C                                       The coefficient near A
            F1 = GAUSS / MAXIMU
C                                       The coefficient near MEAN
            F2 = GAUSS * (XARG - MAXPOS) / SIGMA / SIGMA
C                                       The coefficient near SIGMA
            F3 = GAUSS * (XARG-MAXPOS) * (XARG-MAXPOS)
     *         / SIGMA / SIGMA /SIGMA
            NOBS = NOBS + 1
            SUM = SUM + VALL(I)
            SSQ = SSQ + VALL(I)*VALL(I)
            R(1) = R(1) + RIGHT * F1
            R(2) = R(2) + RIGHT * F2
            R(3) = R(3) + RIGHT * F3
C                                       calculate upper/right triangle
            MATR(1) = MATR(1) + F1 * F1
            MATR(4) = MATR(4) + F1 * F2
            MATR(5) = MATR(5) + F2 * F2
            MATR(7) = MATR(7) + F1 * F3
            MATR(8) = MATR(8) + F2 * F3
            MATR(9) = MATR(9) + F3 * F3
            XARG = XARG + DVAL
 130        CONTINUE
C
         CALL DLESQR (NFIT, NOBS, SUM, SSQ, R, MATR, FITPAR, VX, SSQRES,
     *      VARRES, VARY, FIT, NMSG, IRET)
         IF (IRET.NE.0) THEN
            SIGMA = 0
            MEAN = 0
C
            IF (ROBPRE) THEN
C                                       ROBUST has been run,
C                                       so pick up the ROBUST solution
               SIGMA = SIGROB
               MEAN =  MPOROB
C                                       store SIGMA, MAXPOS, and MAXIMU
C                                       for folowing pixel at the row
               SIGPRE = SIGROB
               MAXPRE = MPOROB
               MPRE = MAXROB
            ELSE
C                                       ROBUST has not been run,
C                                       so return back to run it
               GO TO 5
               END IF
            GO TO 999
            END IF
         VARRES = SQRT(VARRES)
         MAXIMU = MAXIMU + FITPAR(1)
         MAXPOS = MAXPOS + FITPAR(2)
         SIGMA = SIGMA + FITPAR(3)
         MEAN = MAXPOS
         IF ((SIGMA.LE.0) .OR. (MAXIMU.LE.2) .OR. (MAXIMU.GT.1E6)) THEN
            SIGMA = 0
            MEAN = 0
C
            IF (ROBPRE) THEN
C                                       ROBUST has been run,
C                                       so pick up the ROBUST solution
               SIGMA = SIGROB
               MEAN =  MPOROB
C                                       store SIGMA, MAXPOS, and MAXIMU
C                                       for folowing pixel at the row
               SIGPRE = SIGROB
               MAXPRE = MPOROB
               MPRE = MAXROB
            ELSE
C                                       ROBUST has not been run,
C                                       so return back to run it
               GO TO 5
               END IF
            GO TO 999
            END IF
C                                       change of SIGMA is smaller 1%
         SIGNEW = SIGMA
         BIGG = (ABS(SIGNEW-SIGOLD) / SIGOLD) .GT.0.01
         SIGOLD = SIGNEW
         ITER = ITER + 1
         GO TO 100
         END IF
C                                       store SIGMA, MAXPOS, and MAXIMU
      SIGPRE = SIGMA
      MAXPRE = MAXPOS
      MPRE = MAXIMU
C
 999  RETURN
      END
      SUBROUTINE ROBUST (RMSIN, BLANK, NCOUNT, SIGMA, MEAN)
C-----------------------------------------------------------------------
C   Given array of the input data RMSIN, the routine estimates
C   RMSSIGMA) and MEAN using multi iterational process eliminating
C   the far away points
C   Input:
C      RMSIN   R(*)  Array of input data
C      BLANK   L(*)  Array of .TRUE.
C      NCOUNT  I     Number of point in RMSIN
C   Output:
C      SIGMA   R     Solution for the rms
C      MEAN    R     Solution for the mean
C-----------------------------------------------------------------------
      REAL     RMSIN(*), SIGMA, MEAN
      LOGICAL  BLANK(*)
      INTEGER  NCOUNT
C
      REAL     SUM, SUMSQR, RMSI, RMS, RMS3, DRMS, INRMS, INMEAN
      INTEGER  ICOUNT, NCOOLD, NCONEW, ITER, NITER
      INCLUDE 'RMSD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IF (NCOUNT.LE.0) THEN
         SIGMA = FBLANK
         MEAN = FBLANK
         GO TO 999
         END IF
C                                       calculate RMS and MEAN at the
C                                       first iteration
      SUM = 0
      SUMSQR = 0
      DO 190 ICOUNT = 1,NCOUNT
         RMSI = RMSIN(ICOUNT)
         SUM = SUM + RMSI
         SUMSQR = SUMSQR + RMSI*RMSI
 190     CONTINUE
      MEAN = SUM / NCOUNT
      RMS = SQRT (SUMSQR/NCOUNT - MEAN*MEAN)
      INRMS = RMS
      INMEAN = MEAN
C                                       Make iterations subtracting
C                                       the 'bad' value from the SUM
C                                       and SUMSQR
C
C                                       store the old NCOUNT
      NCOOLD = NCOUNT
C                                       make 30 iterations to
C                                       exclude the signal for
C                                       calculating of RMS
      NITER = 30
      DO 220 ITER = 1,NITER
         NCONEW = NCOOLD
C                                       generalise number of RMS
C                                       LK Dec 19, 2007
         RMS3 = RMSCUT * RMS
         DO 210 ICOUNT = 1,NCOUNT
            RMSI = RMSIN(ICOUNT)
C                                       reject all new points.GT.3*RMS
            IF (ABS(RMSI-MEAN).GT.RMS3) THEN
               IF (BLANK(ICOUNT)) THEN
                  SUM = SUM - RMSI
                  SUMSQR = SUMSQR - RMSI*RMSI
                  NCONEW = NCONEW - 1
                  BLANK(ICOUNT) = .FALSE.
                  END IF
            ELSE
               IF (.NOT.BLANK(ICOUNT)) THEN
                  SUM = SUM + RMSI
                  SUMSQR = SUMSQR + RMSI*RMSI
                  NCONEW = NCONEW + 1
                  BLANK(ICOUNT) = .TRUE.
                  END IF
               END IF
 210        CONTINUE
C                                       stop iteration, if the new
C                                       NCOUNT.EQ.the old NCOUNT
         IF (NCONEW.GT.0) THEN
            MEAN = SUM / NCONEW
            DRMS = SUMSQR/NCONEW - MEAN*MEAN
            IF ((NCONEW.EQ.NCOOLD) .OR. (DRMS.LT.0)) GO TO 230
            RMS = SQRT(DRMS)
            NCOOLD = NCONEW
         ELSE
            RMS = INRMS
            MEAN = INMEAN
            END IF
 220     CONTINUE
C
 230  SIGMA = RMS
C
 999  RETURN
      END
      SUBROUTINE DLESQR (NP, N, SUM, SSQ, R, M, X, VX, SSQRES, VARRES,
     *   VARY, FIT, NMSG, IERR)
C-----------------------------------------------------------------------
C     LEASQR does the matrix inversion and other necessary tasks
C     involved in a least squares analysis.
C
C     Given:
C          NP        I     Number of parameters.
C          N         D     The number of observations.
C          SUM       D     Error sum.
C          SSQ       D     Square error sum.
C          R(NP)     D     Results vector.
C
C     Given and returned:
C          M(NP,NP)  D     On input, the upper triangular part contains
C                          the design matrix.  This is not changed.
C                          On output, the lower triangular part contains
C                          the covariance matrix.  Diagonal elements of
C                          the covariance matrix are stored in VX.
C
C     Returned:
C          X(NP)     D     Vector holding the least squares solution.
C          VX(NP)    D     Variance of the best fit parameters.
C          SSQRES    D     Sum of squares of the residuals.
C          VARRES    D     Variance of the residuals.
C          VARY      D     Variance of the error values.
C          FIT       D     Goodness of fit parameter, lies between 0
C                          and 1.
C          IERR      I     Error status, 0 means successful.
C                             1 - nonspecific error return,
C                             2 - insufficient degrees of freedom.
C
C     Called:
C          none
C
C     Algorithm:
C          LU-triangular factorization with scaled partial pivoting.
C          The sub-diagonal triangular matrix contains the scaling
C          factors used at each step in the Gaussian elimination.  Row
C          interchanges are recorded in vectors MXS and SXM.
C             During forward substitution, the pivoting and Gaussian
C          elimination operations performed on matrix M are applied to
C          vector R.  Vector X holds the intermediate result.
C             On backward substitution, successive elements of the
C          solution vector, X, are calculated by substitution of the
C          preceding elements into the equations of the upper triangular
C          factorization of the design matrix.
C
C     Notes:
C       1) Strictly speaking, the design matrix will usually contain
C          rows of zeroes and therefore be singular.  This arises if no
C          observations sensitive to a particular parameter have been
C          done.
C             In practice, any such singularities are ignored and the
C          associated parameters remain undetermined.
C
C       2) The covariance matrix is the inverse of M(i,j) multiplied by
C          the variance of the residuals.  It is obtained by forward and
C          backward substitution on the columns of the unit matrix.
C
C       3) Two statement functions, C, and SC have been employed to
C          partially alleviate the problems posed by passing arrays in
C          FORTRAN.  The design/covariance matrix m(i,j) is copied into
C          the working vector s(i).  This is addressed by using C, and
C          SC in an attempt to make it look like the matrix that it
C          actually represents.
C
C       4) The maximum size problem that LEASQR can handle is set by
C          parameter MX.
C
C     Author:
C          Mark Calabretta, Australia Telescope.
C          Origin; 1988/Sep/29. Code last modified; 1989/Nov/22.
C          Leonid Kogan modified the original version to
C          double precision; 2001/May/30.
C          This version put IERR=1, if "Design matrix inconsistency.."
C-----------------------------------------------------------------------
C     Parameter which determines the maximum size problem.
      INTEGER   MX
      PARAMETER (MX = 50)

      INTEGER   C, I, IERR, ITEMP, J, K, MXS(MX), NF, NP, PIVOT,
     *          SXM(MX), NMSG
      DOUBLE PRECISION   COLMAX, DTEMP, FIT, M(NP,NP), N, R(NP), RESIDU,
     *          RLEN, ROWMAX(MX), S(MX*MX), SC, RTEMP, SSQ, SSQRES, SUM,
     *          VARRES, VARY, VX(NP), W(MX), X(NP)

      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C     Statement functions for array manipulation, see note 3 above.
      C(I,J)  = NP*(I-1) + J
      SC(I,J) = S(C(I,J))
C-----------------------------------------------------------------------
C  Initialize.
C     Anticipate and return immediately on error.
      IERR = 1

C     Initialize arrays.
      DO 40 I = 1, NP
C        Vector which records row interchanges.
         MXS(I) = I

C        The solution and variance vectors.
         X(I)  = 0.0
         VX(I) = 0.0

C        Copy the design matrix and zero the covariance matrix.
         DO 10 J = 1, I-1
            M(I,J) = 0.0
            S(C(I,J)) = M(J,I)
 10         CONTINUE
         DO 20 J = I, NP
            S(C(I,J)) = M(I,J)
 20         CONTINUE

C        Find the maximum absolute element in each row.
         ROWMAX(I) = 0.0
         DO 30 J = 1, NP
            ROWMAX(I) = MAX(ROWMAX(I), ABS(SC(I,J)))
 30         CONTINUE
 40      CONTINUE

      VARY   = 0.0
      SSQRES = 0.0
      VARRES = 0.0
      FIT    = 0.0


C     Find the number of degrees of freedom.
      NF = N
      DO 60 I = 1, NP
         IF (ROWMAX(I).NE.0.0) THEN
            NF = NF - 1
         ELSE IF (R(I).NE.0.0) THEN
            NMSG = NMSG + 1
C           Any row of zeroes must extend to the results vector.
            WRITE (MSGTXT,50) I
 50         FORMAT ('LEASQR: Design matrix inconsistency in row',I4)
            IF (NMSG.EQ.1) CALL MSGWRT (6)
            IERR = 1
            GO TO 999
            END IF
 60      CONTINUE

      IF (NF.LE.1) THEN
         WRITE (MSGTXT,70)
 70      FORMAT ('LEASQR: Insufficient degrees of freedom.')
         CALL MSGWRT (6)
         IERR = 2
         GO TO 999
         END IF

C  Factorize the matrix.
      DO 120 K = 1, NP
C        Check for a row of zeroes.
         IF (ROWMAX(K).EQ.0.0) GO TO 120

C        A non-zero row maximum implies non-zero diagonal element.
         IF (SC(K,K).EQ.0.0) THEN
            WRITE (MSGTXT,50) MXS(K)
            CALL MSGWRT (6)
            IERR = 1
            GO TO 999
C            GO TO 120
            END IF

C        Decide whether to pivot.
         COLMAX = ABS(SC(K,K))/ROWMAX(K)
         PIVOT = K
         DO 80 I = K+1, NP
            IF (ROWMAX(I).NE.0.0) THEN
               IF (ABS(SC(I,K))/ROWMAX(I).GT.COLMAX) THEN
                  COLMAX = ABS(SC(I,K))/ROWMAX(I)
                  PIVOT = I
                  END IF
               END IF
 80         CONTINUE
         IF (PIVOT.GT.K) THEN
C           We must pivot, interchange the rows of the design matrix.
            DO 90 J = 1, NP
               DTEMP = SC(PIVOT,J)
               S(C(PIVOT,J)) = SC(K,J)
               S(C(K,J)) = DTEMP
 90            CONTINUE
C           Don't forget the vector of row maxima.
            DTEMP = ROWMAX(PIVOT)
            ROWMAX(PIVOT) = ROWMAX(K)
            ROWMAX(K) = DTEMP
C           Record the interchange for later use.
            ITEMP = MXS(PIVOT)
            MXS(PIVOT) = MXS(K)
            MXS(K) = ITEMP
            END IF

C        Gaussian elimination.
         DO 110 I = K+1, NP
C           Nothing to do if SC(i,k) is zero.
            IF (SC(I,K).NE.0.0) THEN
C              Save the scaling factor.
               S(C(I,K)) = SC(I,K)/SC(K,K)

C              Subtract rows.
               DO 100 J = K+1, NP
                  S(C(I,J)) = SC(I,J) - SC(I,K)*SC(K,J)
 100              CONTINUE
               END IF
 110        CONTINUE
 120     CONTINUE

C     MXS(i) records which row of M corresponds to row i of SC.
C     SXM(i) records which row of S corresponds to row i of M.
      DO 130 I = 1, NP
         SXM(MXS(I)) = I
 130     CONTINUE


C  Solve the normal equations.
      DO 150 I = 1, NP
C        Forward substitution.
         W(I) = R(MXS(I))
         DO 140 J = 1, I-1
            W(I) = W(I) - SC(I,J)*W(J)
 140        CONTINUE
 150     CONTINUE

      DO 170 I = NP, 1, -1
C        Backward substitution.
         IF (SC(I,I).NE.0.0) THEN
            DO 160 J = I+1, NP
               W(I) = W(I) - SC(I,J)*W(J)
 160           CONTINUE
            W(I) = W(I)/SC(I,I)
            END IF
         X(I) = W(I)
 170     CONTINUE

C     Check that the solution is acceptable.
      RLEN = 0.0
      RESIDU = 0.0
      DO 200 I = 1, NP
         RTEMP = 0.0
         DO 180 J = 1, I-1
            RTEMP = RTEMP + M(J,I)*X(J)
 180        CONTINUE
         DO 190 J = I, NP
            RTEMP = RTEMP + M(I,J)*X(J)
 190        CONTINUE

         RLEN = RLEN + R(I)**2
         RESIDU = RESIDU + (RTEMP - R(I))**2
 200     CONTINUE

      IF (RESIDU.GT.0.001*RLEN) THEN
         WRITE (MSGTXT,210) RESIDU/RLEN
 210     FORMAT ('LEASQR: The solution is discrepant at',E8.1)
         CALL MSGWRT (6)
         GO TO 999
         END IF
C  Determine goodness-of-fit estimates, and statistical errors.
      SSQRES = SSQ
      DO 220 I = 1, NP
         SSQRES = SSQRES - X(I)*R(I)
 220     CONTINUE
      IF (SSQRES.LT.0.0) SSQRES = 0.0
      VARRES = SSQRES/NF
      VARY = (SSQ - SUM*SUM/N)/(N - 1.0)
      FIT = 1.0
      IF (VARY.NE.0.0) FIT = 1.0 - SSQRES/(SSQ - SUM*SUM/N)
C     Determine the covariance matrix.
      DO 280 K = 1, NP
C        Forward substitution affects only that part of W() below the
C        first non-zero entry.
         DO 230 I = 1, SXM(K)-1
            W(I) = 0.0
 230        CONTINUE
         W(SXM(K)) = 1.0

         DO 250 I = SXM(K)+1, NP
C           Forward substitution.
            W(I) = 0.0
            DO 240 J = SXM(K), I-1
               W(I) = W(I) - SC(I,J)*W(J)
 240           CONTINUE
 250        CONTINUE

         DO 270 I = NP, K, -1
            IF (SC(I,I).NE.0.0) THEN
C              Backward substitution.
               DO 260 J = I+1, NP
                  W(I) = W(I) - SC(I,J)*W(J)
 260              CONTINUE
               W(I) = W(I)/SC(I,I)
               END IF

            IF (I.NE.K) THEN
C              Off diagonal elements of the covariance matrix.
               M(I,K) = VARRES*W(I)
            ELSE IF (I.EQ.K) THEN
C              Diagonal elements of the covariance matrix.
               VX(K)  = VARRES*W(I)
               END IF
 270        CONTINUE
 280     CONTINUE
C                                       Successful completion.
      IERR = 0

 999  RETURN
      END
