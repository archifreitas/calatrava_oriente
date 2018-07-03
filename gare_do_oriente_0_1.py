from khepri.autocad import *

p0 = xyz(0,0,0)
inner_length = 5
outer_length = 8
inner_width = 3
outer_width = 4
inner_height = 3
outer_height = 6
grid_length = 40
grid_width = 20
nl = 2
nw = 1

# FILTERS
#Grid
def pts_grid(grid_length, grid_width, nl, nw):
    pts_grid = map_division(lambda a, b: xyz(a,b),
                             0, grid_length, nl,
                             0, grid_width, nw)
    return pts_grid

# Roof Points
def roof_points(p0, inner_length, outer_length, inner_width, outer_width, inner_height, outer_height):
    p1 = xyz(inner_length/2, 0, inner_height)
    p2 = xyz(outer_length/2, outer_width/2, outer_height)
    p3 = xyz(0, inner_width/2, inner_height)
    p4 = xyz(-outer_length/2, outer_width/2, outer_height)
    p5 = xyz(-inner_length/2, 0, inner_height)
    p6 = xyz(-outer_length/2, -outer_width/2, outer_height)
    p7 = xyz(0, -inner_width/2, inner_height)
    p8 = xyz(outer_length/2, -outer_width/2, outer_height)
    return [p1, p2, p3, p4, p5, p6, p7, p8]

# -1 0 1
def sgn(x):
    return -1 if x < 0 else 0 if x == 0 else 1

# List division - Divide distance between two points
def list_div(p, pf, n):
    result = []
    list_x = map_division(lambda a: a, cx(p), cx(pf), n)
    list_y = map_division(lambda a: a, cy(p), cy(pf), n)
    list_z = map_division(lambda a: a, cz(p), cz(pf), n)
    for num in range(len(list_x)):
        result += [xyz(list_x[num], list_y[num], list_z[num])]
    return result

# Pontos Elipse
def pts_elipse(p, a, b, div, t):
    return p + vxz(a * (cos(t) ** 2) ** (1.0 / div) * sgn(cos(t)),
                   b * (sin(t) ** 2) ** (1.0 / div) * sgn(sin(t)))

# Quarter Elipse
def elipse_list(p, a, b, div, n):
    return [pts_elipse(p, a, b, div, t)
           for t in division(0, pi/2, n)]

# ROOF

# Rods  
def rods(p0, p1, p2, beam_height, n):
    result0 = list_div(p0, p1, n)
    result1 = list_div(p0, p2, n)
    for num in range(1, len(result0)):
        move(cylinder(result0[num], 0.02, result1[num]), vz(beam_height))
      
# Medium Rod 
def med_rod(centro_base, topo, beam_height):
    move(cylinder(centro_base, 0.1, topo), vz(beam_height))

# Small Rod
def sml_rod(centro_base, topo, beam_height):
    move(cylinder(centro_base, 0.05, topo), vz(beam_height))

# Roof Plate
def roof_plate(p0, p1, p2, beam_height, n):
    rods(p0, p1, p2, beam_height, n)
    roof_shape(p0, p1, p2, beam_height)

# Roof
def roof(p0, p_list, beam_height, n):
    for num in p_list:
        if p_list.index(num) % 2 == 0:
            med_rod(p0, num, beam_height)
        else:
            sml_rod(p0, num, beam_height)
    for num in range(len(p_list) - 1):
        roof_plate(p0, p_list[num], p_list[num + 1], beam_height, n)
    roof_plate(p0, p_list[-1], p_list[0], beam_height, n)

# BEAMS

def arc_beam(p, inner_width, beam_height, vector, phi, n):
    section = surface_circle(xy(0,0), 0.2)
    curve = spline(elipse_list(p, inner_width / 2, beam_height, 3, n))
    rotate(move(sweep(curve, section), vector), phi)

# Spline'n'Sweep
def arc_beam(p, inner_width, beam_height, vector, phi, n):
    section = surface_circle(xy(0,0), 0.2)
    curve = spline(elipse_list(p, inner_width / 2, beam_height, 3, n))
    rotate(move(sweep(curve, section), vector), phi, p, vz()),

# Beams
def beams(p, inner_width, beam_height, n): 
    arc_beam(p, inner_width, beam_height, vpol(-inner_width/2, 0), 0, n)
    arc_beam(p, inner_width, beam_height, vpol(-inner_width/2, 0), 3 *pi/2, n)
    arc_beam(p, inner_width, beam_height, vpol(-inner_width/2, 0), pi, n)
    arc_beam(p, inner_width, beam_height, vpol(-inner_width/2, 0), pi/2, n)

# Trees
def oriente_trees(grid, inner_width, beam_height, n):
    flattened_list = [y for x in grid for y in x]
    for pt in flattened_list:
        p_list = points(pt, inner_length, outer_length,
                        inner_width, outer_width,
                        inner_height, outer_height)
        roof(pt, p_list, beam_height, n)
        beams(pt, inner_width, beam_height, 6)  