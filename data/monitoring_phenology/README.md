## README for Fuelinex Phenological monitoring

Created by CRD on 25 October 2024

All spreadsheets are both in xlsx and csv. The former is to facilitate data collection by having filtering, frozon columns and row colors. The later is for git version control and input in analyses folder. 

<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

#### 2024_budburst_to_budset.xlsx

2024 is the year. *budburst to budset* is for the first phenological happening in the spring until butset is reached.

**tree_ID**: Species_TreatmentSpring/TreatmentFall_BlocNumber_ReplicateNumber

**bloc**: which bloc is this tree in

**treatment**: what treatment is it subjected to

**species**: which species this tree is

**Notes**: any notes. The format is: *doyXXX: this is the note*. Since this monitoring was done for almost 8 months, I used this format to keep track at wich doy this specific note was taken. Some 

**102, 103, 106, and so on**: phenological stage at this doy

###### The phenological steps used are from 0 to 6, being:

0: Bud dormant

1: Bud flush

2: Leaf emergence, still curly

3: Leaf is unrolled

4: Leaf is completely unfolded

5: Bud start to set, still some green

6: Bud changes color to red or green

*Note: these phenostages are not the right version used in this experiment and are just a guide to help understand the data*

<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

### 2024_senescence.xlsx

**tree_ID**: Species_TreatmentSpring/TreatmentFall_BlocNumber_ReplicateNumber

**bloc**: which bloc is this tree in

**treatment**: what treatment is it subjected to

**species**: which species this tree is

**Notes**: any notes. The format is: *doyXXX: this is the note*. More specifically, we wrote either "**no green leaves**" to define the doy at which the specific tree did not have a single green leaf remaing. However, there was still at least one leaf attached to the tree. "**last leaves fell**" was input in the notes when we noticed that no more leaves were attached to the tree, which can be understood as leaf abscission. 

**248_1, 248_2, 248_3, 248_percent**: 

- 248_1: first chlorophyll measurement 

- 248_2: second chlorophyll measurement

- 248_3: third chlorophyll measurement

- 248_percent: estimated remaining green leaf cover 

*This list goes on until the end of senescence monitoring*



Once a tree didn't have green leaves or no more leaves attached to it, chlorophyll measurements were nas and percent was 0. 
