      SUBROUTINE STARPL (FACTOR, IVOL, CNO, VERS, BLC, TRC, BLUN, BIND,
     *   BCATB, BBLC, BLOCN, PLBUF, IERR)
C-----------------------------------------------------------------------
C! adds to plot mark at coordinates given in an ST (star) file
C# Plot-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2002-2006, 2011
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
C   STARPL plots star positions in a plot file as given by an ST
C   extension file of version VERS.  The ST file contains the center
C   position (RA-DEC, GLON-GLAT, ELON-ELAT) of each star and the
C   "uncertainties" in those star positions.  The plotted plus signs
C   are scaled by these uncertainties and then further scaled by
C   multiplying by FACTOR.
C   Inputs:
C      FACTOR  R         Star scaling factor: <= 0 => no plot.
C      VERS    I         Desired ST file version number: 0 => high
C      IVOL    I         File disk number
C      CNO     I         File catalog number
C      BLC     R(2)      Plot lower left corner (pixels)
C      TRC     R(2)      Plot upper right corner (pixels)
C      BLUN    I         LUN of open backgraound (grey image) 0 -> none
C      BIND    I         FTAB pointer for BLUN 0 -> none
C      BCATR   I(256)    Header for background image
C      BBLC    R(7)      BLC for background image (3 - 7) are used
C      BLOCN   I         LOCNUM for background image - SETLOC must have
C                        been called.  LOCNUM on input is assumed to be
C                        the plot images' location common.
C   In/Out:
C      PLBUF   I(256)    Plot IO buffer
C   Output:
C      IERR    I         Error code: 0 => okay
C                                -1 => there was no ST file
C                                +1 => logical error in ST file
C                                +2 => IO error in ST file
C                                +3 => IO error in plotting
C   Common:
C      DCAT.INC     Image header having the ST file
C      DLOC.INC     Parameters set for LOCNUM and BLOCN
C      DCNT.INC     Buffers used for background image
C      DGPH.INC     Graphics buffer already used to plot grey-scale
C                   background
C   STARPL plots four kinds of marks, and a 24 character star label
C   The types are 0=Cross, 1=Ellipse, 2=Box, 3=triangle, 4=diamond,
C   5=pentagon, 6=hexigon, 7=septigon, 8=octigon, 9=9-gon, etc
C   20=cross with a gap in center
C-----------------------------------------------------------------------
      REAL      FACTOR, BLC(2), TRC(2), BBLC(7)
      INTEGER   VERS, IVOL, CNO, BLUN, BIND, BCATB(256), BLOCN,
     *   PLBUF(256), IERR
C                                       Max Numb Columns, Label length
      INTEGER   MXSTCL, MXSTLB, MXLINE
      PARAMETER (MXSTCL=7, MXSTLB=24, MXLINE=72)
C
      INTEGER   BUFFER(512), STKOLS(MXSTCL), STNUMV(MXSTCL), ISTRNO,
     *   IV, I, JTRIM, TABLUN, IER, NPL, SCRTCH(50), NST, IST, NCHAR,
     *   LRNO, STTYPE, IROUND, SLOCN, BPOS, BVALUE, BCUT
      REAL      DELJ, DELN, AX(5), AY(5), POSANG, TX, TY, LBBLC(7),
     *   LBTRC(7), STWID(3)
      DOUBLE PRECISION DX, DY, COSPA, SINPA, COSDEC, STXY(2)
      LOGICAL   DOBACK, DOTHIS, BLACKN, DOLABL
      CHARACTER LABEL*24, TXTMSG*80
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DCNT.INC'
      DATA TABLUN /27/
C-----------------------------------------------------------------------
      IERR = 0
      IF (FACTOR.EQ.0.0) GO TO 999
C                                       Is there an ST file
      IERR = -1
      CALL FNDEXT ('ST', CATBLK, I)
      IF (I.LE.0) GO TO 999
      IV = VERS
      IF ((IV.LE.0) .OR. (IV.GT.I)) IV = I
      DOBACK = (BLUN.GT.0) .AND. (BIND.GT.0) .AND. (GPHDOD)
      IF (DOBACK) BCUT = GPHCUT * GPHTHI + (1.0-GPHCUT) * GPHTLO
      SLOCN = LOCNUM
      CALL RCOPY (7, BBLC, LBBLC)
      CALL RCOPY (7, BBLC, LBTRC)
C                                       Open ST table file
      CALL STINI ('READ', BUFFER, IVOL, CNO, IV, CATBLK, TABLUN, ISTRNO,
     *   STKOLS, STNUMV, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, IV
         GO TO 990
         END IF
      NST = BUFFER(5)
C                                       comment in plot file
      WRITE (TXTMSG,1095) NST, IV
      CALL GCOMNT (-1, TXTMSG, PLBUF, IERR)
      IF (IERR.NE.0) GO TO 985
C                                       I guess we can do it now
      NPL = 0
      LRNO = 0
      DO 100 IST = 1,NST
         CALL TABST ('READ', BUFFER, ISTRNO, STKOLS, STNUMV, STXY,
     *      STWID, STTYPE, LABEL, IER)
         IF (IER.LT.0) GO TO 100
         IF (IER.NE.0) GO TO 975
C                                       Convert to radians
         POSANG = STWID(3) * TWOPI / 360.0
         COSPA = COS (POSANG)
         SINPA = SIN (POSANG)
         COSDEC = ABS (COS (STXY(2)*DG2RAD))
C                                       Calc center of star pos
         DOTHIS = DOBACK
         IF (DOBACK) THEN
            LOCNUM = BLOCN
            CALL XYPIX (STXY(1), STXY(2), LBBLC(1), LBBLC(2), IER)
            LBBLC(1) = IROUND (LBBLC(1))
            LBBLC(2) = IROUND (LBBLC(2))
            DOTHIS = (IER.EQ.0) .AND. (LBBLC(1).GE.1) .AND.
     *         (LBBLC(2).GT.1) .AND. (LBBLC(1).LE.BCATB(KINAX)) .AND.
     *         (LBBLC(2).LE.BCATB(KINAX+1))
            LBTRC(1) = LBBLC(1)
            LBTRC(2) = LBBLC(2)
            LOCNUM = SLOCN
            END IF
         CALL XYPIX (STXY(1), STXY(2), AX(1), AY(1), IER)
C                                       If star not in plot, get next
         IF ((BLC(1).GT.AX(1)) .OR. (BLC(2).GT.AY(1)) .OR.
     *       (TRC(1).LT.AX(1)) .OR. (TRC(2).LT.AY(1))) GO TO 100
C                                       Move and Put Text on plot
C                                       Calculate ends of star mark
         DELJ = 0.5 * STWID(1) * ABS(FACTOR)
         DELN = 0.5 * STWID(2) * ABS(FACTOR)
         IF (CORTYP(LOCNUM).EQ.1) THEN
            COSDEC = ABS (COS (STXY(2)*DG2RAD))
            DX = STXY(1) + DELJ*SINPA/COSDEC
            DY = STXY(2) + DELJ*COSPA
         ELSE IF (CORTYP(LOCNUM).EQ.2) THEN
            COSDEC = ABS (COS (STXY(1)*DG2RAD))
            DX = STXY(1) + DELJ*COSPA
            DY = STXY(2) + DELJ*SINPA/COSDEC
         ELSE
            DX = STXY(1)
            DY = STXY(2) + DELJ
            END IF
         CALL XYPIX (DX, DY, AX(2), AY(2), IER)
         IF (IER.NE.0) GO TO 100
         DX = 2.0 * STXY(1) - DX
         DY = 2.0 * STXY(2) - DY
         CALL XYPIX (DX, DY, AX(3), AY(3), IER)
         IF (IER.NE.0) GO TO 100
         IF (CORTYP(LOCNUM).EQ.1) THEN
            DX = STXY(1) - DELN*COSPA/COSDEC
            DY = STXY(2) + DELN*SINPA
         ELSE IF (CORTYP(LOCNUM).EQ.2) THEN
            DX = STXY(1) + DELN*SINPA
            DY = STXY(2) - DELN*COSPA/COSDEC
         ELSE
            DX = STXY(1) - DELN
            DY = STXY(2)
            END IF
         CALL XYPIX (DX, DY, AX(4), AY(4), IER)
         IF (IER.NE.0) GO TO 100
         DX = 2.0 * STXY(1) - DX
         DY = 2.0 * STXY(2) - DY
         CALL XYPIX (DX, DY, AX(5), AY(5), IER)
         IF (IER.NE.0) GO TO 100
C                                       If type > 22, draw an ellipse
         IF (STTYPE.GT.23) STTYPE = 3
C                                       check the background value
         IF (DOTHIS) THEN
            CALL DBINIT (BLUN, BIND, BCATB, LBBLC, LBTRC, JBUFSZ, IBUFF,
     *         IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1090) 'INIT', IERR
               GO TO 990
               END IF
            CALL MDISK ('READ', BLUN, BIND, IBUFF, BPOS, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1090) 'READ', IERR
               GO TO 990
               END IF
            CALL GSCALE (GPHFUN, GPHRNG, 1, 1, BUFF(BPOS), BVALUE)
            BLACKN = BVALUE.GT.BCUT
         ELSE
            BLACKN = .FALSE.
            END IF
C                                        If the label is requested
         NCHAR = JTRIM (LABEL)
         DOLABL = (FACTOR.LT.0.0) .AND. (NCHAR.GT.0)
         IF (DOLABL) THEN
C                                        IF a box or no mark
C                                        Then top right corner
            IF ((STTYPE.EQ.4) .OR. (STTYPE.LE.0)) THEN
               TX =  AX(1) + (AX(2) - AX(3) - AX(4) + AX(5))/2
               TY =  AY(1) + (AY(2) - AY(3) - AY(4) + AY(5))/2
C                                        Else right edge of mark
            ELSE
               TX = AX(5)
               TY = AY(5)
               END IF
C                                       If label starts in the plot
C                                       Move and Put Text on plot
            IF ((BLC(1).LE.TX) .AND. (BLC(2).LE.TY) .AND.
     *         (TRC(1).GE.TX) .AND. (TRC(2).GE.TY)) THEN
               CALL GPOS (TX, TY, PLBUF, IERR)
               IF (IERR.NE.0) GO TO 985
               IF (BLACKN) THEN
                  I = 2
               ELSE
                  I = 1
                  END IF
               CALL GICHAR (I, NCHAR, 0, 0., 0., LABEL, PLBUF, IERR)
               IF (IERR.NE.0) GO TO 985
               END IF
            END IF
C                                       plot the point
         IF ((STTYPE.GT.0) .OR. ((.NOT.DOLABL) .AND. (STTYPE.EQ.0)))
     *      THEN
            CALL PNTPLT (STTYPE, AX, AY, BLC, TRC, BLACKN, .FALSE.,
     *         PLBUF, IERR)
            IF (IERR.NE.0) GO TO 985
            NPL = NPL + 1
         ELSE
            IF (DOLABL) NPL = NPL + 1
            END IF
 100     CONTINUE
      IERR = 0
      WRITE (MSGTXT,1100) NPL, NST, IV
      CALL MSGWRT (2)
      GO TO 985
C                                       Error print and close
 975  WRITE (MSGTXT,1975) IER
      IERR = 2
 980  CALL MSGWRT (8)
 985  CALL TABIO ('CLOS', 0, IST, SCRTCH, BUFFER, IER)
      GO TO 999
C                                       Error print
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I5,' OPENING ST FILE VERSION',I4)
 1020 FORMAT ('CANNOT FIND COLUMNS IN STAR TABLE: HAS IMAGE',
     *   ' BEEN TRANSPOSED?')
 1090 FORMAT ('STARPL: ERROR ',A,'ING BACKGROUND IMAGE =',I4)
 1095 FORMAT ('Plotting',I6,' possible positions from ST table version',
     *   I4)
 1100 FORMAT ('Plotted',I6,' of',I6,' positions from ST table version',
     *   I4)
 1975 FORMAT ('IO ERROR',I5,' IN STAR TABLE FILE')
      END
