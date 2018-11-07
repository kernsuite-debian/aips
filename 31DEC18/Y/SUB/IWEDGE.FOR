      SUBROUTINE IWEDGE (TYPE, SIDE, CHAN, IX0, IY0, NCOL, NROW, DRANGE,
     *   RBUFF, IBUFF, IERR)
C-----------------------------------------------------------------------
C! draw wedges (incl zeros) on TV
C# TV-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2011
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
C   draws wedges on the TV using values that are all 0 or are part or
C   all of an images intensity range.
C   Inputs:
C      TYPE     I      0 => zero the wedge
C                      1 => use full range of image intensities
C                      2 => use clipped range of image intensities
C      SIDE     I      0, 1, 2, 3 -> left, bottom, right, top
C      CHAN     I      TV channel to use
C      IX0      I      BLC on TV of wedge in X
C      IY0      I      BLC on TV of wedge in Y
C      NCOL     I      Number of columns in wedge: X extent
C      NROW     I      Number of rows in wedge: Y extent
C      DRANGE   R(2)   Intensity range to display: 0,0 => use type
C                        drange(1) <= drange(2) => use values
C   Output:
C      RBUFF    R(*)   Scratch buffer
C      IBUFF    I(*)   Scratch buffer - may be same as RBUFF IF and only
C                      IF TYPE >= 0.
C      IERR     I      Error code
C   In/out in common:
C      CATBLK (DCAT.INC) TV image header in, wedge image header out
C-----------------------------------------------------------------------
      INTEGER   TYPE, SIDE, CHAN, IX0, IY0, NCOL, NROW, IERR
      REAL      DRANGE(2), RBUFF(*), IBUFF(*)
C
      CHARACTER LINTYP*2, WETYP(4)*4
      LOGICAL   WASYNC, VERTCL
      INTEGER   IPL, I, IER, JJ
      REAL      X, Y
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA WETYP /'WELL', 'WEBB', 'WERR', 'WETT'/
C-----------------------------------------------------------------------
      IF (NCOL.LE.1) NCOL = 2
      IF (NROW.LE.1) NROW = 2
      WASYNC = ISYNCH.EQ.0
      VERTCL = (SIDE.EQ.0) .OR. (SIDE.EQ.2)
      JJ = NCOL
      IF (VERTCL) JJ = NROW
C                                       zero:
      IF (TYPE.EQ.0) THEN
         CALL FILL (NCOL, 0, IBUFF)
C                                       Wedge
      ELSE
         IF (CATR(IRRAN).EQ.CATR(IRRAN+1)) THEN
            CATR(IRRAN) = CATR(IRRAN) - 0.5
            CATR(IRRAN+1) = CATR(IRRAN+1) + 0.5
            END IF
C                                       IMPWEDGE: clip min/max
         IF (TYPE.EQ.2) THEN
            CATR(KRDMN) = CATR(IRRAN)
            CATR(KRDMX) = CATR(IRRAN+1)
            END IF
C                                       DRANGE argument
         IF (((DRANGE(1).NE.0.0) .OR. (DRANGE(2).NE.0.0)) .AND.
     *      (DRANGE(1).LE.DRANGE(2))) THEN
            IF (DRANGE(1).EQ.DRANGE(2)) THEN
               CATR(KRDMN) = 0.999 * DRANGE(1)
               CATR(KRDMX) = 1.001 * DRANGE(2)
            ELSE
               CATR(KRDMN) = DRANGE(1)
               CATR(KRDMX) = DRANGE(2)
               END IF
            END IF
C                                       avoid zero divide
         IF (CATR(KRDMX).EQ.CATR(KRDMN)) THEN
            CATR(KRDMX) = CATR(IRRAN+1)
            CATR(KRDMN) = CATR(IRRAN)
            END IF
C                                       Numbers into pixel form
         X = CATR(KRDMN)
         Y = CATR(KRDMX)
         Y = (Y - X)
         IF (JJ.GT.1) Y = Y / (JJ - 1)
         IF (Y.EQ.0.0) THEN
            CATR(IRRAN) = 0.0
            CATR(IRRAN+1) = 10.0
            END IF
         DO 20 I = 1,JJ
            RBUFF(I) = X + (I-1.0)*Y
 20         CONTINUE
C                                        Scale buffer like image
         CALL H2CHR (2, 1, CATH(IITRA), LINTYP)
         CALL ISCALE (LINTYP, MAXINT, CATR(IRRAN), JJ, 1, RBUFF, IBUFF)
         END IF
C                                        Reset Catlg record
C                                        meaningful part
      CATBLK(IICOR  ) = IX0
      CATBLK(IICOR+1) = IY0
      CATBLK(IICOR+2) = IX0 + NCOL - 1
      CATBLK(IICOR+3) = IY0 + NROW - 1
      CALL COPY (4, CATBLK(IICOR), CATBLK(IIWIN))
C                                        clear rest for zeroing
      IF (TYPE.EQ.0) THEN
         CALL RFILL (5, HBLANK, CATH(KHIMN))
         CATBLK(KIIMS) = 0
         CATBLK(IIVOL) = 0
         CATBLK(IICNO) = 0
         CATR(KRDMX) = 0.0
         CATR(KRDMN) = 0.0
         CATR(IRRAN) = 0.0
         CATR(IRRAN+1) = 0.0
         CALL RFILL (2, HBLANK, CATH(KHBUN))
         CATBLK(KINIT) = 0
         CALL CHR2H (2, 'ZZ', KHPTYO, CATH(KHPTY))
C                                        except for image wedge
      ELSE
         CALL CHR2H (2, 'WE', KHPTYO, CATH(KHPTY))
         END IF
      JJ = 0
      IF (VERTCL) JJ = 1
      I = 2 * KICTPN
      CALL RFILL (I, HBLANK, CATH(KHCTP))
      CALL CHR2H (4, WETYP(SIDE+1), 1, CATH(KHCTP+I-2))
      CALL RCOPY (2, CATH(KHBUN), CATH(KHCTP+2*JJ))
      CATR(KRCRP+JJ) = (CATBLK(IICOR+JJ) + CATBLK(IICOR+JJ+2)) / 2.0
      CATD(KDCRV+JJ) = (CATR(KRDMX) + CATR(KRDMN)) / 2.0
      CATR(KRCIC+JJ) = (CATR(KRDMX) - CATR(KRDMN)) / (CATBLK(IICOR+JJ+2)
     *   - CATBLK(IICOR+JJ))
      CATR(KRCIC+1-JJ) = 0.0
      CATR(KRCRP+1-JJ) = CATBLK(IICOR+1-JJ) - 1
      CATD(KDCRV+1-JJ) = 0.0
C                                       Load to TV
      IF (WASYNC) CALL YHOLD ('ONNN', IER)
      IPL = IY0 - 1
      IF (TYPE.EQ.0) THEN
         IPL = IPL + NROW
         I = IX0 + NCOL - 1
         CALL YFILL (CHAN, IX0, IY0, I, IPL, 0, IBUFF, IERR)
C                                       horizontal wedge
      ELSE IF (.NOT.VERTCL) THEN
         DO 30 I = 1,NROW
            IPL = IPL + 1
            CALL YIMGIO ('WRIT', CHAN, IX0, IPL, 0, NCOL, IBUFF, IERR)
            IF (IERR.NE.0) GO TO 50
 30        CONTINUE
      ELSE
         DO 40 I = 1,NROW
            IPL = IPL + 1
            CALL RFILL (NCOL, IBUFF(I), RBUFF)
            CALL YIMGIO ('WRIT', CHAN, IX0, IPL, 0, NCOL, RBUFF, IERR)
            IF (IERR.NE.0) GO TO 50
 40         CONTINUE
         END IF
C                                       resume display
 50   IF (WASYNC) CALL YHOLD ('OFFF', IER)
C                                        Update catalog
      IF ((I.GT.1) .OR. (IERR.EQ.0)) CALL YCWRIT (CHAN, CATBLK(IICOR),
     *   CATBLK, IBUFF, IER)
C
 999  RETURN
      END
