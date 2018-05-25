LOCAL INCLUDE 'OBPLT.INC'
      INTEGER BUFFER(256)
      COMMON /FBUFF/ BUFFER
C                                       POPS variable names
      INTEGER NPARM
      PARAMETER (NPARM = 22)
      HOLLERITH      XINNAM(3), XINCLA(2)
      REAL           XINSEQ, XINDIS, XINVER
      HOLLERITH      XOPCOD, XOPTYP
      REAL           XAPARM(10), XLTYPE, XDOTV
      COMMON /INPARM/ XINNAM, XINCLA,
     *                XINSEQ, XINDIS, XINVER,
     *                XOPCOD, XOPTYP,
     *                XAPARM, XLTYPE, XDOTV
C                                       internal variable names
      CHARACTER       INNAME*12, INCLAS*6, OPCODE*4, OPTYPE*4
      COMMON /CHRCOM/ INNAME,    INCLAS,   OPCODE,   OPTYPE
      INTEGER         INDISK, INCAT, INSEQ, INVERS, LTYPE
      LOGICAL         DOTV
      REAL            APARM(10)
      COMMON /NUMCOM/ INDISK, INCAT, INSEQ, INVERS, DOTV, APARM, LTYPE
C
C                                       table info is stored here
      INCLUDE 'INCS:POBV.INC'
      INTEGER TABBUF(512)
      INTEGER N
      INTEGER OBKOLS(MAXOBC), OBNUMB(MAXOBC)
      COMMON /TBUF/ TABBUF, N, OBKOLS, OBNUMB

LOCAL END
      PROGRAM OBPLT
C-----------------------------------------------------------------------
C! Plots one column of an OB table against another
C# TASK VLBI PLOT UV
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 2003, 2005, 2007, 2012, 2014
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
C   OBPLT plots any two columns of an OB table against each other
C   Inputs:
C   AIPS adverb  Prg. name.          Description.
C   INNAME         INNAME        Name of the file to be sorted
C   INCLASS        INCLAS        Class of the file to be sorted.
C   INSEQ          INSEQ         Seq. number of file to be sorted.
C   INDISK         INDISK        Disk number of file to be sorted.
C   INVERS         INVERS        Vers. number of OB table to be plotted.
C   OPCODE         OPCODE        X-Column to plot.
C   OPTYPE         OPTYPE        Y-Column to plot.
C   APARM          APARM         APARM(1-2) can be used to specify
C                                explicitly the limits on the X-axis
C                                instead of self-scaling
C                                APARM(3-4) can be used to specify
C                                explicitly the limits on the X-axis
C                                instead of self-scaling
C   LTYPE          LTYPE         Used to specify how axes are labelled.
C                                As per standard AIPS conventions
C                                [see HELP LTYPE]
C   DOTV           DOTV          If DOTV=1, plot appears on TV,
C                                if DOTV=0, plot goes to PL file.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET, IERR
      INCLUDE 'OBPLT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA PRGM /'OBPLT '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      IRET = 18
      CALL OBPLIN (PRGM, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Read OB table and extract
C                                       wanted columns
      IRET = 17
      CALL PREPOB (IERR)
      IF (IERR.NE.0) GO TO 990
      IRET = 0
C                                       Close down files
 990  CALL DIE (IRET, BUFFER)
C
 999  STOP
      END
      SUBROUTINE OBPLIN (PRGM, JERR)
C-----------------------------------------------------------------------
C   OBPLIN gets input parameters for OBPLT
C   Inputs: PRGM   C*6       Task name
C   Output: JERR   I         Error code: 0 => ok, else quit
C-----------------------------------------------------------------------
      CHARACTER PRGM*6, STAT*4, UTYPE*2
      INTEGER  JERR, TABLUN, IROUND, IERR
      LOGICAL   T
      INCLUDE 'OBPLT.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA TABLUN /27/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Init IO characteristics
      CALL ZDCHIN (T)
      CALL VHDRIN
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      CALL GTPARM (PRGM, NPARM, RQUICK, XINNAM, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         JERR = 8
         RQUICK = .FALSE.
         GO TO 999
         END IF
C                                       Crunch input parameters.
      INSEQ  = IROUND (XINSEQ)
      INDISK = IROUND (XINDIS)
      INVERS = IROUND (XINVER)
C                                       Characters
      CALL H2CHR (12, 1, XINNAM, INNAME)
      CALL H2CHR (6, 1, XINCLA, INCLAS)
      CALL H2CHR (4, 1, XOPCOD, OPCODE)
      CALL H2CHR (4, 1, XOPTYP, OPTYPE)
C                                       Reals
C                                       axis labelling handle, see
C                                       LTYPE adverb inside AIPS
      LTYPE = IROUND (XLTYPE)
      IF (MOD(ABS(LTYPE), 100).EQ.0) THEN
         IF (LTYPE.LT.0) THEN
            LTYPE = (LTYPE/100)*100 - 3
         ELSE
            LTYPE = (LTYPE/100)*100 + 3
            END IF
         END IF
      XLTYPE = LTYPE
C                                       manually set limits on X and
C                                       Y axes using APARM(1-2) and
C                                       APARM(3-4) respectively
      APARM(1) = XAPARM(1)
      APARM(2) = XAPARM(2)
      APARM(3) = XAPARM(3)
      APARM(4) = XAPARM(4)
C                                       Logicals
      DOTV = XDOTV.GT.0.0
C                                       Restart AIPS.
      IF (RQUICK) CALL RELPOP (JERR, BUFFER, IERR)
C                                       Get CATBLK.
      INCAT = 1
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', INDISK, INCAT, INNAME, INCLAS, INSEQ, UTYPE,
     *   NLUSER, STAT, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR, INNAME, INCLAS, INSEQ, INDISK, NLUSER
         CALL MSGWRT (8)
         JERR = 5
         GO TO 999
         END IF
      CALL CATIO ('READ', INDISK, INCAT, CATBLK, 'REST', BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR
         CALL MSGWRT (8)
         JERR = 5
         GO TO 999
         END IF
      NCFILE = 1
      FVOL(1) = INDISK
      FCNO(1) = INCAT
      FRW(1) = 0
C                                       max number of points to plot from
C                                       table
C
C                                       Init table
      CALL OBINI ('READ', TABBUF, INDISK, INCAT, INVERS, CATBLK, TABLUN,
     *   N, OBKOLS, OBNUMB, JERR)
      IF (JERR.NE.0) GO TO 999
C                                       count number of rows
      N = TABBUF(5)
C                                       done!
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('OBPLIN: ERROR',I7,' OBTAINING INPUT PARAMETERS')
 1010 FORMAT ('OBPLIN: ERROR',I7,' FINDING ',A12,'.',A6,'.',I4,
     *               ' DISK=',I3,' USID=',I5)
 1020 FORMAT ('OBPLIN: ERROR',I7,' COPYING CATBLK ')
      END
      SUBROUTINE PREPOB (IRET)
C-----------------------------------------------------------------------
C   PREPOB allocates enough dynamic memory to plot all possible rows of
C   the OB table; additionally, PREPOB loads the data arrays and invokes
C   the plotting routine PLOTOB.  These must be invoked here since the
C   data arrays are created here.
C   Inputs through local include OBPLT.INC
C      N        I        Number of rows in OB table
C   Outputs
C      IRET     I        Error code: 0 => ok
C                           not 0 => not ok
C-----------------------------------------------------------------------
      INTEGER    IRET
C
      INCLUDE 'OBPLT.INC'
      LONGINT    OX, OY
      REAL       X(2), Y(2)
      INTEGER    NMEM, IERR
C-----------------------------------------------------------------------
C                                       be optimistic
      IRET = 0
C                                       get memory for X array
      NMEM = N
      IF (NMEM.GT.2) THEN
         NMEM = ( ( NMEM - 1 )/256 + 1 ) * 256
         NMEM = (NMEM - 1) / 1024 + 1
         CALL ZMEMRY ('GET', 'PREPOB', NMEM, X, OX, IRET)
         IF (IRET.NE.0) GO TO 100
      ELSE
         OX = 0
         END IF
C                                       get memory for X array
      NMEM = N
      IF (NMEM.GT.2) THEN
         NMEM = ( ( NMEM - 1 )/256 + 1 ) * 256
         NMEM = (NMEM - 1) / 1024 + 1
         CALL ZMEMRY ('GET', 'PREPOB', NMEM, Y, OY, IRET)
         IF (IRET.NE.0) GO TO 100
      ELSE
         OY = 0
         END IF
C                                       load arrays
      CALL LOADOB (X(1+OX), Y(1+OY), IRET)
C                                       plot arrays
      IF (IRET.EQ.0) CALL PLOTOB (X(1+OX), Y(1+OY), IRET)
C                                       free memory
 100  CALL ZMEMRY ('FRAL', 'PREPOB', NMEM, X, OX, IERR)
C
 999  RETURN
      END
      SUBROUTINE LOADOB (X, Y, IRET)
C-----------------------------------------------------------------------
C   LOADOB gets data arrays from OB table
C   Inputs:
C      N           I      (via common) total number of rows in OB table
C   Output:
C      X(N)        I      X axis data array
C      Y(N)        I      Y axis data array
C      N           I      (via common) actual # of data points loaded
C      IRET        I      Error code: 0 => ok, else quit
C-----------------------------------------------------------------------
      INTEGER IRET
      INCLUDE 'OBPLT.INC'
C                                       get FBLANK value
      INCLUDE 'INCS:DDCH.INC'
      REAL X(N), Y(N)
      INTEGER OBROW, OBANT, OBSUB, M
      REAL OBANGL(3), OBECL(4), OBORI, XTEMP, YTEMP
      DOUBLE PRECISION OBTIME, OBPOS(3), OBVEL(3)
C-----------------------------------------------------------------------
C                                       start at row 1
      OBROW = 1
C                                       initialize #points loaded
      M = 0
C                                       loop over rows of OB table
 300  CONTINUE
         IF (OBROW.GT.N) GO TO 390
         CALL TABOB ('READ', TABBUF, OBROW, OBKOLS, OBNUMB,
     *      OBANT, OBSUB, OBTIME, OBPOS, OBVEL, OBANGL, OBECL, OBORI,
     *      IRET)
         IF (IRET.EQ.4) GO TO 390
C                                       find x point here
         CALL GETOBV (OPCODE, OBANT, OBSUB, OBTIME,
     *      OBPOS, OBVEL, OBANGL, OBECL, OBORI, XTEMP)
C                                       find y point here
         CALL GETOBV (OPTYPE, OBANT, OBSUB, OBTIME,
     *      OBPOS, OBVEL, OBANGL, OBECL, OBORI, YTEMP)
C                                       load arrays here
         IF ((XTEMP.NE.FBLANK).AND.(YTEMP.NE.FBLANK)) THEN
            M = M + 1
            X(M) = XTEMP
            Y(M) = YTEMP
            END IF
         GO TO 300
 390     CONTINUE
C                                       reset counter to #points loaded
      N = M
C                                       done!
 999  RETURN
      END
      SUBROUTINE GETOBV (AXTYPE, OBANT, OBSUB, OBTIME,
     *   OBPOS, OBVEL, OBANGL, OBECL, OBORI, AXVAL)
C-----------------------------------------------------------------------
C   GETOBV extracts the requested data value for an OB table record
C   Inputs:
C      AXTYPE    C*4    Axis type label - user supplied
C             These quantities are as returned by TABOB
C      OBANT     I      Antenna number for this OB record
C      OBSUB     I      Subarray number for this OB record
C      OBTIME    R
C      OBPOS(3)  D(3)
C      OBVEL(3)  D(3)
C      OBANGL(3) R(3)
C      OBECL(4)  R(4)
C      OBORI     R      Parallactic angle
C   Outputs:
C      AXVAL     R     Value for the axis type requested
C-----------------------------------------------------------------------
      CHARACTER AXTYPE*4
      INTEGER OBANT, OBSUB
      REAL OBANGL(3), OBECL(4), OBORI
      DOUBLE PRECISION OBTIME, OBPOS(3), OBVEL(3)
      REAL AXVAL
C-----------------------------------------------------------------------
      IF (AXTYPE.EQ.'ANT ') AXVAL = OBANT
      IF (AXTYPE.EQ.'SUB ') AXVAL = OBSUB
      IF (AXTYPE.EQ.'TIME') AXVAL = OBTIME * 24.0
      IF (AXTYPE.EQ.'XPOS') AXVAL = OBPOS(1)
      IF (AXTYPE.EQ.'YPOS') AXVAL = OBPOS(2)
      IF (AXTYPE.EQ.'ZPOS') AXVAL = OBPOS(3)
      IF (AXTYPE.EQ.'XVEL') AXVAL = OBVEL(1)
      IF (AXTYPE.EQ.'YVEL') AXVAL = OBVEL(2)
      IF (AXTYPE.EQ.'ZVEL') AXVAL = OBVEL(3)
      IF (AXTYPE.EQ.'ANG1') AXVAL = OBANGL(1)
      IF (AXTYPE.EQ.'ANG2') AXVAL = OBANGL(2)
      IF (AXTYPE.EQ.'ANG3') AXVAL = OBANGL(3)
      IF (AXTYPE.EQ.'ECL1') AXVAL = OBECL(1)
      IF (AXTYPE.EQ.'ECL2') AXVAL = OBECL(2)
      IF (AXTYPE.EQ.'ECL3') AXVAL = OBECL(3)
      IF (AXTYPE.EQ.'ECL4') AXVAL = OBECL(4)
      IF (AXTYPE.EQ.'ORIE') AXVAL = OBORI
      IF (AXTYPE.EQ.'DIST') THEN
         AXVAL = OBPOS(1)**2 + OBPOS(2)**2 + OBPOS(3)**2
         AXVAL = SQRT(AXVAL)
         END IF
      IF (AXTYPE.EQ.'VEL') THEN
         AXVAL = OBVEL(1)**2 + OBVEL(2)**2 + OBVEL(3)**2
         AXVAL = SQRT(AXVAL)
         END IF
      IF (AXTYPE.EQ.'BECL') AXVAL = OBECL(1) * 24.0
      IF (AXTYPE.EQ.'EECL') AXVAL = OBECL(2) * 24.0
      RETURN
      END
      SUBROUTINE GETOBU (AXTYPE, AXUNIT)
C-----------------------------------------------------------------------
C   GETOBU extracts the units of the requested data value for an OB
C      table record
C   Inputs:
C      AXTYPE    C*4    Axis type label - user supplied
C             These quantities are as returned by TABOB
C   Outputs:
C      AXUNIT   C*20   Units for the axis type requested
C-----------------------------------------------------------------------
      CHARACTER AXUNIT*20, AXTYPE*4
C-----------------------------------------------------------------------
      IF (AXTYPE.EQ.'ANT ') AXUNIT = 'STAT'
      IF (AXTYPE.EQ.'SUB ') AXUNIT = 'SUBA'
      IF (AXTYPE.EQ.'TIME') AXUNIT = 'Hours'
      IF (AXTYPE.EQ.'XPOS') AXUNIT = 'Meters'
      IF (AXTYPE.EQ.'YPOS') AXUNIT = 'Meters'
      IF (AXTYPE.EQ.'ZPOS') AXUNIT = 'Meters'
      IF (AXTYPE.EQ.'XVEL') AXUNIT = 'Meters/Second'
      IF (AXTYPE.EQ.'YVEL') AXUNIT = 'Meters/Second'
      IF (AXTYPE.EQ.'ZVEL') AXUNIT = 'Meters/Second'
      IF (AXTYPE.EQ.'ANG1') AXUNIT = 'Sun Angle'
      IF (AXTYPE.EQ.'ANG2') AXUNIT = 'Sun Angle'
      IF (AXTYPE.EQ.'ANG3') AXUNIT = 'Sun Angle'
      IF (AXTYPE.EQ.'ECL1') AXUNIT = 'Begin Eclipse'
      IF (AXTYPE.EQ.'ECL2') AXUNIT = 'End Eclipse'
      IF (AXTYPE.EQ.'ECL3') AXUNIT = ' '
      IF (AXTYPE.EQ.'ECL4') AXUNIT = ' '
      IF (AXTYPE.EQ.'ORIE') AXUNIT = 'Degrees'
      IF (AXTYPE.EQ.'DIST') AXUNIT = 'Meters'
      IF (AXTYPE.EQ.'VEL')  AXUNIT = 'Meters/Second'
      IF (AXTYPE.EQ.'BECL') AXUNIT = 'Hours'
      IF (AXTYPE.EQ.'EECL') AXUNIT = 'Hours'
      RETURN
      END
      SUBROUTINE PLOTOB (X, Y, IRET)
C-----------------------------------------------------------------------
C   This routine will plot the data in YARRAY versus the data in XARRAY
C   Input:
C      X       R(N)     Array of X-values
C      Y       R(N)     Array of Y-values
C   Output:
C      IRET    I        Termination code (0 => ok)
C   Input from common:
C      DOTV    L        If true then plot to TV dev., else PL file.
C-----------------------------------------------------------------------
      INTEGER IRET
C                                       the dimension of X and Y are
C                                       carried in OBPLT.INC
      INCLUDE 'OBPLT.INC'
      REAL X(N), Y(N)
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DPLD.INC'
      INCLUDE 'INCS:DMSG.INC'
      CHARACTER LABOVE(5)*80, LBELOW(5)*80, LXUNIT*20, LYUNIT*20
      REAL BLC(2), TRC(2), XDIV, YDIV, XWIDTH, YWIDTH
      REAL PGBLC(2), PGTRC(2), XMIN, XMAX, YMIN, YMAX, XPFRAC, YPFRAC
      INTEGER NX, NY, IGTYPE, ITVORG(2), IGRCHN, ITVCHN, IERR, NABOVE,
     *   NBELOW
      INTEGER PLTBUF(256)
C----------------------------------------------------------------------
C                                       Initialisation
      IRET = 0
      CALL FILL (2, 0, ITVORG)
      IGRCHN = 1
      ITVCHN = 1
C                                       New page
      IGTYPE = 1
      CALL PLINIT (IGTYPE, INDISK, INCAT, CATBLK, NPARM, XINNAM,
     *   DOTV, IGRCHN, ITVCHN, ITVORG, PLTBUF, IERR)
      IF (IERR.NE.0) THEN
         IRET = 1
         WRITE (MSGTXT,1020) IERR
         GO TO 990
         END IF
C                                       Define plotting page in pixels
      PGBLC(1) = 0.0
      PGBLC(2) = 0.0
      PGTRC(1) = 1000.0
      PGTRC(2) = 1000.0
C                                       required first line
      NABOVE = 2
      WRITE (LABOVE(1), 1030) INNAME, INCLAS, INSEQ, INDISK
      WRITE (LABOVE(2), 1031) INVERS, OPTYPE, OPCODE
      NBELOW = 0
C                                       get X-axis units
      CALL GETOBU (OPCODE, LXUNIT)
C                                       get Y-axis units
      CALL GETOBU (OPTYPE, LYUNIT)
      NX = 1
      NY = 1
C                                       these numbers need tweaking.
      XPFRAC = 0.35
C                                       this used to be 0.9
      YPFRAC = 0.35
      XDIV = NX + (NX + 0.5) * XPFRAC
      YDIV = NY + (NY + 1.0) * YPFRAC
      XWIDTH = (PGTRC(1) - PGBLC(1)) / XDIV
      YWIDTH = (PGTRC(2) - PGBLC(2)) / YDIV
C                                       define lower left of plot
      BLC(1) = PGBLC(1) + XWIDTH * XPFRAC
      BLC(2) = PGBLC(2) + YWIDTH * YPFRAC
C                                       define upper right of plot
      TRC(1) = BLC(1) + XWIDTH
      TRC(2) = BLC(2) + YWIDTH
C                                       Determine axis range
C                                       did user set limits manually?
      IF (APARM(1).LT.APARM(2)) THEN
         XMIN = APARM(1)
         XMAX = APARM(2)
      ELSE
         CALL PLRANG (X, N, 1.05, 0.0, XMIN, XMAX)
         END IF
      IF (APARM(3).LT.APARM(4)) THEN
         YMIN = APARM(3)
         YMAX = APARM(4)
      ELSE
         CALL PLRANG (Y, N, 1.05, 0.0, YMIN, YMAX)
         END IF
C                                       write header and plot axes
      CALL PLAXES (BLC, TRC, XMIN, XMAX, YMIN, YMAX, LTYPE, LXUNIT,
     *   LYUNIT, NABOVE, NBELOW, LABOVE, LBELOW, 5, .FALSE., IERR)
      IF (IERR.NE.0) THEN
         IRET = 2
         WRITE (MSGTXT,1040) IERR
         GO TO 990
      END IF

C                                       Plot points
      CALL PLPNT (X, Y, N, 1, 0.03, IERR)
      IF (IERR.NE.0) THEN
         IRET = 3
         WRITE (MSGTXT,1220) IERR
         GO TO 990
         END IF
C                                       End page
      CALL PLCLOS (INDISK, INCAT, CATBLK, PLTBUF, IRET)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
      CALL PLCLOS (INDISK, INCAT, CATBLK, PLTBUF, IRET)
C                                       Exit
 999  RETURN
C----------------------------------------------------------------------
 1020 FORMAT ('PLOTOB: ERROR',I5,' RETURNED BY PLINIT')
 1030 FORMAT ('OB table plot for ',A12,'.',A6,'.',I4,'.',I4)
 1031 FORMAT ('OB # ',I4,' : ',A4,' vs ',A4)
 1040 FORMAT ('PLOTOB: ERROR',I5,' RETURNED BY PLAXES')
 1220 FORMAT ('PLOTOB: ERROR',I5,' RETURNED BY PLPNT')
      END
