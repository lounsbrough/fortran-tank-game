PROGRAM TANK
IMPLICIT NONE
CHARACTER(1), ALLOCATABLE :: grid(:,:)
CHARACTER(10) :: player_name, hill_level, wind_level
CHARACTER(1) :: response, menu_command
INTEGER :: i, j, k, l, m, nrows, ncolumns, x(23), y(23), tank_column, tank_row, t_c1, t_c2, h_s_score(5)
INTEGER :: n_shots, angle, velocity, wind_speed, wind_direction, hit_count, score, clock, win, bomb, load_space
CHARACTER(10) :: h_s_name(5), h_s_terrain(5), h_s_wind(5)
REAL :: h, rise, h_w, w_l
CALL resolution(nrows,ncolumns)
ALLOCATE (grid(nrows,ncolumns))
grid = " "
grid(nrows,:) = "="
CALL system("cls")
WRITE (*,*)
WRITE (*,*)
WRITE (*,*)
WRITE (*,*) "                             //"
WRITE (*,*) "                            //" 
WRITE (*,*) "                          /\/\"
WRITE (*,*) "                         /\/\/"
WRITE (*,*) "                        /\/\/"
WRITE (*,*) "            ___________/\/\/______"
WRITE (*,*) "     ______(______________________)_____"
WRITE (*,*) "    ||                                 ||"
WRITE (*,*) "     \\\                               //"
WRITE (*,*) "      \\\      WELCOME to TANK!       //"
WRITE (*,*) "       \\\                           //"
WRITE (*,*) "        \\\_________________________//"
WRITE (*,*) "         (/\/\/\/\/\/\/\/\/\/\/\/\/)"
WRITE (*,*)
WRITE (*,*)
WRITE (*,*)
WRITE (*,*) "      Enter your Name (10 letter Maximum)"
WRITE (*,*)
READ (*,*) player_name
DO
CALL system("cls")
WRITE (*,*)
WRITE (*,*) "Choose a Level for Wind:  (L)ow  |  (M)edium  |  (H)igh"
WRITE (*,*)
READ (*,*) response
IF (response == "L" .OR. response == "l") THEN
w_l = 0.01
wind_level = "Low"
EXIT
ELSE IF (response == "M" .OR. response == "m") THEN
w_l = 0.03
wind_level = "Medium"
EXIT
ELSE IF (response == "H" .OR. response == "h") THEN
w_l = 0.07
wind_level = "High"
EXIT
ELSE
WRITE (*,*)
WRITE (*,*) "This is an invalid response."
WRITE (*,*)
WRITE (*,*) "Enter..."
WRITE (*,*)
READ (*,*)
END IF
END DO
h = 0.9*nrows
l = 0
m = 0
h_w = random_number()*(ncolumns/10)+10.0
DO k = 1, ncolumns
load_space = ncolumns/10
CALL system("cls")
WRITE (*,*)
WRITE (*,*) "Generating Terrain..."
WRITE (*,*)
IF (REAL(k)/ncolumns >= 0.9) THEN
DO i = 1, 9*load_space
WRITE (*,301,ADVANCE="NO") "."
END DO
WRITE(*,302) "90%"
ELSE IF (REAL(k)/ncolumns >= 0.8) THEN
DO i = 1, 8*load_space
WRITE (*,301,ADVANCE="NO") "."
END DO
WRITE(*,302) "80%"
ELSE IF (REAL(k)/ncolumns >= 0.7) THEN
DO i = 1, 7*load_space
WRITE (*,301,ADVANCE="NO") "."
END DO
WRITE(*,302) "70%"
ELSE IF (REAL(k)/ncolumns >= 0.6) THEN
DO i = 1, 6*load_space
WRITE (*,301,ADVANCE="NO") "."
END DO
WRITE(*,302) "60%"
ELSE IF (REAL(k)/ncolumns >= 0.5) THEN
DO i = 1, 5*load_space
WRITE (*,301,ADVANCE="NO") "."
END DO
WRITE(*,302) "50%"
ELSE IF (REAL(k)/ncolumns >= 0.4) THEN
DO i = 1, 4*load_space
WRITE (*,301,ADVANCE="NO") "."
END DO
WRITE(*,302) "40%"
ELSE IF (REAL(k)/ncolumns >= 0.3) THEN
DO i = 1, 3*load_space
WRITE (*,301,ADVANCE="NO") "."
END DO
WRITE(*,302) "30%"
ELSE IF (REAL(k)/ncolumns >= 0.2) THEN
DO i = 1, 2*load_space
WRITE (*,301,ADVANCE="NO") "."
END DO
WRITE(*,302) "20%"
ELSE IF (REAL(k)/ncolumns >= 0.1) THEN
DO i = 1, load_space
WRITE (*,301,ADVANCE="NO") "."
END DO
WRITE(*,302) "10%"
END IF
301 FORMAT (A1)
302 FORMAT (A3)
IF (l == 0) THEN
DO
rise = random_number()*2.0-1.0
IF (rise <= 0) THEN
m = m + 1
EXIT
END IF
END DO
END IF
IF (l == 1) THEN
DO
rise = random_number()*2.0-1.0
IF (rise >= 0) THEN
m = m + 1
EXIT
END IF
END DO
END IF
IF (m >= NINT(h_w)) THEN
  IF (l == 0) THEN
    l = 1
    h_w = random_number()*(ncolumns/10)+10.0
  ELSE IF (l == 1) THEN
    l = 0
    h_w = random_number()*(ncolumns/10)+10.0
  END IF
  m = 0
END IF
IF (NINT(h)+NINT(rise) <= 3) rise=0
IF (NINT(h)+NINT(rise) >= nrows-3) rise=0
DO i = NINT(h)+NINT(rise),nrows-1
  grid(i,k) = "~"
END DO
h = h + rise
END DO
CALL system("cls")
DO i = 1, nrows
  WRITE (*,*) (grid(i,j), j = 1, ncolumns)
END DO
tank_column = NINT(random_number()*ncolumns*0.9)+5
DO i = 1, nrows
IF (grid(i,tank_column) == "~" .OR. grid(i,tank_column) == "=") THEN
grid(i-1,tank_column) = "T"
grid(i-1,tank_column-1) = " "
grid(i-1,tank_column+1) = " "
tank_row = i-1
EXIT
END IF
END DO
k = 0
l = 0
t_c1 = 1000
t_c2 = 1000
DO
j = NINT(random_number()*ncolumns*0.9)+5
IF (ABS(tank_column-j) > (ncolumns*0.15) .AND. ABS(t_c1-j) > (ncolumns*0.05) .AND. ABS(t_c2-j) > (ncolumns*0.05)) THEN
  DO i = 1, nrows
    IF (grid(i,j) == "~" .OR. grid(i,j) == "=") THEN
      grid(i-1,j) = "X"
      grid(i-1,j+1) = "X"
      grid(i-2,j) = "X"
      grid(i-2,j+1) = "X"
      EXIT
    END IF
  END DO
k = k + 1
l = 1
END IF
IF (k == 1 .AND. l == 1) THEN
  t_c1 = j
  l = 0
ELSE IF (k == 2 .AND. l == 1) THEN
  t_c2 = j
  l = 0
END IF
IF (k == 3) EXIT
END DO
CALL system("cls")
DO i = 1, nrows
  WRITE (*,*) (grid(i,j), j = 1, ncolumns)
END DO
WRITE (*,*)
WRITE (*,*) "READY!"
WRITE (*,*)
WRITE (*,*) "Press Enter to Continue..."
WRITE (*,*)
READ (*,*)
n_shots = 50
angle = 70
velocity = 70
wind_speed = random_number()*24
wind_direction = random_number()*2.0
win = 0
bomb = 0
DO
IF (n_shots == 0) EXIT
IF (win == 1) EXIT
IF (win == -1) EXIT
DO
CALL game_display(n_shots,angle,velocity,wind_speed,wind_direction,score,bomb)
WRITE (*,*)
WRITE (*,*) "Select an Option:   (A)ngle   |   (V)elocity   |   (F)ire"
WRITE (*,*)
READ (*,*) menu_command
IF (menu_command == "A" .OR. menu_command == "a") EXIT
IF (menu_command == "V" .OR. menu_command == "v") EXIT
IF (menu_command == "F" .OR. menu_command == "f") EXIT
IF (menu_command == "B" .OR. menu_command == "b") EXIT
CALL system("cls")
WRITE (*,*)
WRITE (*,*) "This is not a valid option."
WRITE (*,*)
WRITE (*,*) "Enter..."
WRITE (*,*)
READ (*,*)
END DO
IF (menu_command == "A" .OR. menu_command == "a") THEN
DO
  CALL system("cls")
  WRITE (*,*)
  WRITE (*,18) angle
  18 FORMAT (" Your CURRENT Angle: ", I3)
  WRITE (*,*)
  WRITE (*,*) "Enter Angle   |   (0 > angle > 180)"
  WRITE (*,*)
  READ (*,*) angle
  IF (angle > 0 .AND. angle < 180) THEN
    EXIT
  ELSE
    WRITE (*,*)
    WRITE (*,*) "That angle's not in range."
    WRITE (*,*)
    WRITE (*,*) "*(0 < angle < 180)*"
    WRITE (*,*)
    WRITE (*,*) "Enter..."
    WRITE (*,*)
    READ (*,*)
  END IF
END DO
ELSE IF (menu_command == "V" .OR. menu_command == "v") THEN
DO
  CALL system("cls")
  WRITE (*,*)
  WRITE (*,19) velocity
  19 FORMAT (" Your CURRENT Velocity: ", I3)
  WRITE (*,*)
  WRITE (*,*) "Enter Velocity   |   (20 <= velocity <= 100)"
  WRITE (*,*)
  READ (*,*) velocity
  IF (velocity >= 20 .AND. velocity <= 100) THEN
    EXIT
  ELSE
    WRITE (*,*)
    WRITE (*,*) "That velocity is not in range."
    WRITE (*,*)
    WRITE (*,*) "*(20 <= velocity <= 100)*"
    WRITE (*,*)
    WRITE (*,*) "Enter..."
    WRITE (*,*)
    READ (*,*)
  END IF
END DO
ELSE IF (menu_command == "B" .OR. menu_command == "b") THEN
  IF (bomb == 0) THEN
    bomb = 1
  ELSE
    bomb = 0
  END IF
ELSE IF (menu_command == "F" .OR. menu_command == "f") THEN
  CALL projectile_simulation(angle,velocity,wind_speed,wind_direction,grid,tank_column,win,w_l,bomb)
  n_shots = n_shots - 1
  wind_speed = random_number()*23
  wind_direction = NINT(random_number())
  bomb = 0
  END IF
END DO
CALL system("cls")
DO i = 1, nrows
  WRITE (*,*) (grid(i,j), j = 1, ncolumns)
END DO
WRITE (*,*)
IF (win == 0) THEN
  WRITE (*,*) "Maybe Next Time ", TRIM(player_name), "..."
  WRITE (*,*)
  score = 0
ELSE IF (win == 1) THEN
  WRITE (*,*) "You Won ", TRIM(player_name), "!"
  WRITE (*,*)
  WRITE (*,23) score
  23 FORMAT (" Final Score: ", I3)
  WRITE (*,*)
ELSE IF (win == -1) THEN
  WRITE (*,*) "Way to go man! :)"
  WRITE (*,*)
  WRITE (*,*) "You blew up your Tank!"
  WRITE (*,*)
  score = 0
END IF
OPEN (unit=23,file="High-Scores.dat",status="old")
READ (23,*) response, response, response
READ (23,*) response, response, response, response, response
DO i = 1, 5
  READ (23,*) l, h_s_name(i), h_s_score(i), h_s_terrain(i), h_s_wind(i)
END DO
l = 0
DO i = 1, 5
  IF (score > h_s_score(i)) THEN
    l = i
    EXIT
  END IF
END DO
IF (l /= 0) THEN
DO i = 5, l+1, -1
  h_s_score(i) = h_s_score(i-1)
  h_s_name(i) = h_s_name(i-1)
  h_s_terrain(i) = h_s_terrain(i-1)
  h_s_wind(i) = h_s_wind(i-1)
END DO
h_s_score(l) = score
h_s_name(l) = player_name
h_s_terrain(l) = hill_level
h_s_wind(l) = wind_level
CLOSE (23)
OPEN (unit=23,file="High-Scores.dat",status="old")
READ (23,*) response, response, response
READ (23,*) response, response, response, response, response
WRITE (23,*)
DO i = 1, 5
WRITE (23,32) i, h_s_name(i), h_s_score(i), h_s_terrain(i), h_s_wind(i)
32 FORMAT (I4,6X,A10,I4,2X,2(4X,A6))
END DO
WRITE (*,51) l
51 FORMAT ("You made it on the High Scores List!   (RANK: ", I1, ")")
WRITE (*,*)
WRITE (*,*) "            TANK   HIGH   SCORES"
WRITE (*,*)
WRITE (*,*) "  RANK   NAME        SCORE   TERRAIN   WIND"
WRITE (*,*)
DO i = 1, 5
WRITE (*,32) i, h_s_name(i), h_s_score(i), h_s_terrain(i), h_s_wind(i)
END DO
WRITE (*,*)
END IF
STOP
CONTAINS
!/~\-/~\!
SUBROUTINE projectile_simulation(angle,velocity,wind_speed,wind_direction,grid,tank_column,win,w_l,bomb)
IMPLICIT NONE
INTEGER, INTENT (IN) :: angle, velocity, wind_speed, wind_direction, tank_column, bomb
INTEGER :: i, j, k, hit_count
INTEGER, INTENT (OUT) :: win
REAL :: speed(100000)
REAL, INTENT (IN) :: w_l
REAL :: time(100000), range(100000), altitude(100000), accel_x, sim_angle, time_final, altitude_step, range_step
INTEGER :: step_final
CHARACTER(1), INTENT (IN OUT) :: grid(nrows,ncolumns)	
i = 1
time = 0
IF (wind_direction == 0) accel_x = -REAL(wind_speed)*w_l
IF (wind_direction == 1) accel_x = REAL(wind_speed)*w_l
sim_angle = REAL(angle)*3.14159/180.0
altitude_step = 510/(REAL(tank_row)*2.0)
range_step = 1260/(REAL(ncolumns)*2.0)
DO
  time(i) = REAL(i)*0.03
  range(i) = REAL(velocity)*COS(sim_angle)*time(i) + 0.5*accel_x*time(i)**2
  altitude(i) = REAL(velocity)*SIN(sim_angle)*time(i) - 0.5*9.81*time(i)**2
  speed(i) = SQRT((REAL(velocity)*COS(sim_angle)+accel_x*time(i))**2+(REAL(velocity)*SIN(sim_angle)-9.81*time(i))**2)
  IF (altitude(i) < -510) THEN
    EXIT
  END IF
  i = i + 1
END DO
step_final = i
time_final = time(i)
hit_count = 0
DO k = 1, step_final	
  j = FLOOR(range(k)/range_step)
  i = FLOOR(tank_row - 1 - altitude(k)/altitude_step)
  IF (i > 0 .AND. i < (nrows-1) .AND. j+tank_column > 1 .AND. j+tank_column < ncolumns) THEN	  
  IF (grid(i,j+tank_column) == " ") THEN	  
    grid(i,j+tank_column) = "*"
    CALL game_display(n_shots,angle,velocity,wind_speed,wind_direction,score,bomb)
    CALL PAUSE(speed(i))
  ELSE IF (grid(i,j+tank_column) == "~") THEN
    grid(i,j+tank_column) = "*"
    CALL game_display(n_shots,angle,velocity,wind_speed,wind_direction,score,bomb)
    CALL PAUSE(150.0)
    IF (grid(i-1,j+tank_column) == "X") hit_count = 1
    IF (i-1 /= nrows) grid(i-1,j+tank_column) = "*"
    CALL game_display(n_shots,angle,velocity,wind_speed,wind_direction,score,bomb)
    CALL PAUSE(150.0)
    IF (grid(i-1,j+tank_column+1) == "X") hit_count = 1
    IF (i-1 < nrows) grid(i-1,j+tank_column+1) = "*"
    CALL game_display(n_shots,angle,velocity,wind_speed,wind_direction,score,bomb)
    CALL PAUSE(150.0)
    IF (grid(i,j+tank_column+1) == "X") hit_count = 1
    IF (i < nrows) grid(i,j+tank_column+1) = "*"
    CALL game_display(n_shots,angle,velocity,wind_speed,wind_direction,score,bomb)
    CALL PAUSE(150.0)
    IF (grid(i+1,j+tank_column+1) == "X") hit_count = 1
    IF (i+1 < nrows) grid(i+1,j+tank_column+1) = "*"
    CALL game_display(n_shots,angle,velocity,wind_speed,wind_direction,score,bomb)
    CALL PAUSE(150.0)
    IF (grid(i+1,j+tank_column) == "X") hit_count = 1
    IF (i+1 < nrows) grid(i+1,j+tank_column) = "*"
    CALL game_display(n_shots,angle,velocity,wind_speed,wind_direction,score,bomb)
    CALL PAUSE(150.0)
    IF (grid(i+1,j+tank_column-1) == "X") hit_count = 1
    IF (i+1 < nrows) grid(i+1,j+tank_column-1) = "*"
    CALL game_display(n_shots,angle,velocity,wind_speed,wind_direction,score,bomb)
    CALL PAUSE(150.0)
    IF (grid(i,j+tank_column-1) == "X") hit_count = 1
    IF (i < nrows) grid(i,j+tank_column-1) = "*"
    CALL game_display(n_shots,angle,velocity,wind_speed,wind_direction,score,bomb)
    CALL PAUSE(150.0)
    IF (grid(i,j+tank_column-1) == "X") hit_count = 1
    IF (i-1 < nrows) grid(i-1,j+tank_column-1) = "*"
    CALL game_display(n_shots,angle,velocity,wind_speed,wind_direction,score,bomb)
    IF (bomb == 1) THEN
      CALL PAUSE(100.0)
      DO l = -2, 2
      DO m = -2, 2
        IF (grid(i+l,j+tank_column+m) == "X") hit_count = 1
        IF (i+l < nrows) grid(i+l,j+tank_column+m) = "*"
      END DO
    END DO
    CALL game_display(n_shots,angle,velocity,wind_speed,wind_direction,score,bomb)
    CALL PAUSE(100.0)
    DO l = -3, 3
      DO m = -3, 3
        IF (grid(i+l,j+tank_column+m) == "X") hit_count = 1
        IF (i+l < nrows) grid(i+l,j+tank_column+m) = "*"
      END DO
    END DO
    CALL game_display(n_shots,angle,velocity,wind_speed,wind_direction,score,bomb)
    CALL PAUSE(100.0)
    DO l = -2, 2
      DO m = -2, 2
        IF (grid(i+l,j+tank_column+m) == "*") grid(i+l,j+tank_column+m) = " "
      END DO
    END DO
    CALL game_display(n_shots,angle,velocity,wind_speed,wind_direction,score,bomb)
    CALL PAUSE(100.0)
    DO l = -4, 4
      DO m = -4, 4
        IF (grid(i+l,j+tank_column+m) == "X") hit_count = 1
        IF (i+l < nrows) grid(i+l,j+tank_column+m) = "*"
      END DO
    END DO
    CALL game_display(n_shots,angle,velocity,wind_speed,wind_direction,score,bomb)
    CALL PAUSE(100.0)
    DO l = -3, 3
      DO m = -3, 3
        IF (grid(i+l,j+tank_column+m) == "*") grid(i+l,j+tank_column+m) = " "
      END DO
    END DO
    CALL game_display(n_shots,angle,velocity,wind_speed,wind_direction,score,bomb)
    CALL PAUSE(100.0)
    DO l = -5, 5
      DO m = -5, 5
        IF (grid(i+l,j+tank_column+m) == "X") hit_count = 1
        IF (i+l < nrows) grid(i+l,j+tank_column+m) = "*"
      END DO
    END DO
    CALL game_display(n_shots,angle,velocity,wind_speed,wind_direction,score,bomb)
    CALL PAUSE(100.0)
    DO l = -4, 4
      DO m = -4, 4
        IF (grid(i+l,j+tank_column+m) == "*") grid(i+l,j+tank_column+m) = " "
      END DO
    END DO
    CALL game_display(n_shots,angle,velocity,wind_speed,wind_direction,score,bomb)
    END IF
    EXIT
  ELSE IF (grid(i,j+tank_column) == "X") THEN
    grid(i,j+tank_column) = "*"
    CALL game_display(n_shots,angle,velocity,wind_speed,wind_direction,score,bomb)
    CALL PAUSE(150.0)
    IF (i-1 /= nrows) grid(i-1,j+tank_column) = "*"
    CALL game_display(n_shots,angle,velocity,wind_speed,wind_direction,score,bomb)
    CALL PAUSE(150.0)
    IF (i-1 /= nrows) grid(i-1,j+tank_column+1) = "*"
    CALL game_display(n_shots,angle,velocity,wind_speed,wind_direction,score,bomb)
    CALL PAUSE(150.0)
    IF (i /= nrows) grid(i,j+tank_column+1) = "*"
    CALL game_display(n_shots,angle,velocity,wind_speed,wind_direction,score,bomb)
    CALL PAUSE(150.0)
    IF (i+1 /= nrows) grid(i+1,j+tank_column+1) = "*"
    CALL game_display(n_shots,angle,velocity,wind_speed,wind_direction,score,bomb)
    CALL PAUSE(150.0)
    IF (i+1 /= nrows) grid(i+1,j+tank_column) = "*"
    CALL game_display(n_shots,angle,velocity,wind_speed,wind_direction,score,bomb)
    CALL PAUSE(150.0)
    IF (i+1 /= nrows) grid(i+1,j+tank_column-1) = "*"
    CALL game_display(n_shots,angle,velocity,wind_speed,wind_direction,score,bomb)
    CALL PAUSE(150.0)
    IF (i /= nrows) grid(i,j+tank_column-1) = "*"
    CALL game_display(n_shots,angle,velocity,wind_speed,wind_direction,score,bomb)
    CALL PAUSE(150.0)
    IF (i-1 /= nrows) grid(i-1,j+tank_column-1) = "*"
    CALL game_display(n_shots,angle,velocity,wind_speed,wind_direction,score,bomb)
    hit_count = 1
    IF (bomb == 1) THEN
      CALL PAUSE(100.0)
      DO l = -2, 2
      DO m = -2, 2
        IF (i+l < nrows) grid(i+l,j+tank_column+m) = "*"
      END DO
    END DO
    CALL game_display(n_shots,angle,velocity,wind_speed,wind_direction,score,bomb)
    CALL PAUSE(100.0)
    DO l = -3, 3
      DO m = -3, 3
        IF (i+l < nrows) grid(i+l,j+tank_column+m) = "*"
      END DO
    END DO
    CALL game_display(n_shots,angle,velocity,wind_speed,wind_direction,score,bomb)
    CALL PAUSE(100.0)
    DO l = -2, 2
      DO m = -2, 2
        IF (grid(i+l,j+tank_column+m) == "*") grid(i+l,j+tank_column+m) = " "
      END DO
    END DO
    CALL game_display(n_shots,angle,velocity,wind_speed,wind_direction,score,bomb)
    CALL PAUSE(100.0)
    DO l = -4, 4
      DO m = -4, 4
        IF (i+l < nrows) grid(i+l,j+tank_column+m) = "*"
      END DO
    END DO
    CALL game_display(n_shots,angle,velocity,wind_speed,wind_direction,score,bomb)
    CALL PAUSE(100.0)
    DO l = -3, 3
      DO m = -3, 3
        IF (grid(i+l,j+tank_column+m) == "*") grid(i+l,j+tank_column+m) = " "
      END DO
    END DO
    CALL game_display(n_shots,angle,velocity,wind_speed,wind_direction,score,bomb)
    CALL PAUSE(100.0)
    DO l = -5, 5
      DO m = -5, 5
        IF (i+l < nrows) grid(i+l,j+tank_column+m) = "*"
      END DO
    END DO
    CALL game_display(n_shots,angle,velocity,wind_speed,wind_direction,score,bomb)
    CALL PAUSE(100.0)
    DO l = -4, 4
      DO m = -4, 4
        IF (grid(i+l,j+tank_column+m) == "*") grid(i+l,j+tank_column+m) = " "
      END DO
    END DO
    CALL game_display(n_shots,angle,velocity,wind_speed,wind_direction,score,bomb)
    END IF
    EXIT
  END IF
  ELSE IF (i == (nrows-1) .AND. j+tank_column > 1 .AND. j+tank_column < ncolumns) THEN
    grid(i,j+tank_column) = "*"
    CALL game_display(n_shots,angle,velocity,wind_speed,wind_direction,score,bomb)
    CALL PAUSE(150.0)
    IF (grid(i-1,j+tank_column) == "X") hit_count = 1
    grid(i-1,j+tank_column) = "*"
    CALL game_display(n_shots,angle,velocity,wind_speed,wind_direction,score,bomb)
    CALL PAUSE(150.0)
    IF (grid(i-1,j+tank_column+1) == "X") hit_count = 1
    grid(i-1,j+tank_column+1) = "*"
    CALL game_display(n_shots,angle,velocity,wind_speed,wind_direction,score,bomb)
    CALL PAUSE(150.0)
    IF (grid(i,j+tank_column+1) == "X") hit_count = 1
    grid(i,j+tank_column+1) = "*"
    CALL game_display(n_shots,angle,velocity,wind_speed,wind_direction,score,bomb)
    CALL PAUSE(150.0)
    IF (grid(i-1,j+tank_column) == "X") hit_count = 1
    grid(i-1,j+tank_column) = "*"
    CALL game_display(n_shots,angle,velocity,wind_speed,wind_direction,score,bomb)
    CALL PAUSE(150.0)
    IF (grid(i-1,j+tank_column+1) == "X") hit_count = 1
    grid(i-1,j+tank_column+1) = "*"
    CALL game_display(n_shots,angle,velocity,wind_speed,wind_direction,score,bomb)
    CALL PAUSE(150.0)
    IF (grid(i,j+tank_column+1) == "X") hit_count = 1
    grid(i,j+tank_column+1) = "*"
    CALL game_display(n_shots,angle,velocity,wind_speed,wind_direction,score,bomb)
    CALL PAUSE(150.0)
    IF (grid(i,j+tank_column-1) == "X") hit_count = 1
    grid(i,j+tank_column-1) = "*"
    CALL game_display(n_shots,angle,velocity,wind_speed,wind_direction,score,bomb)
    CALL PAUSE(150.0)
    IF (grid(i-1,j+tank_column-1) == "X") hit_count = 1
    grid(i-1,j+tank_column-1) = "*"
    CALL game_display(n_shots,angle,velocity,wind_speed,wind_direction,score,bomb)
    EXIT
  END IF
END DO
l = 0
k = 0
DO i = 1, nrows
DO j = 1, ncolumns
  IF (grid(i,j) == "." .OR. grid(i,j) == "*") grid(i,j) = " "
  IF (grid(i,j) == "X") l = l + 1
  IF (grid(i,j) == "T") k = 1
END DO
END DO
IF (l == 0) win = 1
IF (k == 0) win = -1
WRITE (*,*)
IF (win == -1) THEN
  WRITE (*,*) "*KABOOM*"
ELSE
  IF (hit_count == 0) WRITE (*,*) "~MISS~"
  IF (hit_count == 1) WRITE (*,*) "HIT!"
END IF
WRITE (*,*)
READ (*,*)
END SUBROUTINE projectile_simulation
!/~\-/~\!
SUBROUTINE game_display(n_shots,angle,velocity,wind_speed,wind_direction,score,bomb)
IMPLICIT NONE
INTEGER, INTENT (IN) :: n_shots, angle, velocity, wind_speed, wind_direction, bomb
INTEGER, INTENT (OUT) :: score
CHARACTER(4) :: wind_display
CALL system("cls")
DO i = 1, nrows
  WRITE (*,*) (grid(i,j), j = 1, ncolumns)
END DO
IF (wind_direction == 0) wind_display = "WEST"
IF (wind_direction == 1) wind_display = "EAST"
score = 100.99999 - (0.333*(50-n_shots))**1.637
IF (bomb == 0) THEN
  WRITE (*,*)
  WRITE (*,18) angle, velocity, wind_speed, wind_display, n_shots, score
  18 FORMAT (1X,"Angle: ",I3,3X,"Velocity: ",I3,3X,"Wind: ",I2,1X,A4,3X,"Shots Remaining: ",I2,3X,"Current Score: ",I3)
ELSE
  WRITE (*,*)
  WRITE (*,180) angle, velocity, wind_speed, wind_display, n_shots, score, "B-O-M-B"
  180 FORMAT (1X,"Angle: ",I3,3X,"Velocity: ",I3,3X,"Wind: ",I2,1X,A4,3X,"Shots Remaining: ",I2,3X,"Current Score: ",I3,5X,A7)
END IF
END SUBROUTINE game_display
!/~\-/~\!
FUNCTION random_seed()
IMPLICIT NONE
REAL (kind=8), PARAMETER :: max_seed = 1.d6
REAL (kind=8) :: time, random_seed
random_seed = 0.d0
DO
  CALL cpu_time(time)
  random_seed = random_seed + time
  IF (random_seed > max_seed) EXIT
END DO
random_seed = DABS(DSIN(random_seed))*max_seed
RETURN
END FUNCTION random_seed
!/~\-/~\!
FUNCTION random_number()
IMPLICIT NONE
REAL (kind=8) :: seed
REAL (kind=8) :: random_number
seed = random_seed()
seed = MOD(8121.d0*seed + 28411.d0,134456.d0)
random_number = seed/134456.d0
RETURN
END FUNCTION random_number
!/~\-/~\!
SUBROUTINE Pause(speed)
IMPLICIT NONE
INTEGER :: clock
REAL :: l, m
REAL, INTENT (IN) :: speed
l = 0
DO
  CALL SYSTEM_CLOCK(clock)
  IF (l == 0) m = clock
  l = clock
  IF (ABS(l-m) > 5000/speed) EXIT
END DO
END SUBROUTINE
!/~\-/~\!
SUBROUTINE resolution(nrows,ncolumns)
IMPLICIT NONE
INTEGER, INTENT (OUT) :: nrows, ncolumns
nrows = 50
ncolumns = 200
END SUBROUTINE resolution
END PROGRAM TANK