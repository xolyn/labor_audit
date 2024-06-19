def cleanQLabel(val):
    val=str(val)

# Emergency Preparedness:
    if val in [
'are escape routes free of obstruction',
'are the aisles and emergency exits accessible, unobstructed and unlocked during working hours, including overtime',
'are the emergency exits accessible and unobstructed during working hours, including overtime',
'are the emergency exits accessible, unobstructed and unlocked during working hours, including overtime',
'are the emergency exits accessible, unobstructed and unlocked during working hours, including overtime? [differentiation question]',
'are the emergency exits and escape routes accessible, unobstructed and unlocked during working hours, including overtime',
'are the emergency exits unlocked during working hours, including overtime',
'are floors and corridors in line with legal requirements']:
        return "accessible emergency exit?"
    
    elif val in ['are any of the emergency exits inaccessible, obstructed, or locked during working hours, including overtime',
'are any of the emergency exits or escape routes inaccessible, obstructed, or locked during working hours, including overtime',
'are the emergency exits inaccessible and obstructed during working hours, including overtime',
'are the emergency exits inaccessible or obstructed during working hours, including overtime',
'are the emergency exits locked during working hours, including overtime',]:
        return "inaccessible emergency exit?"

    elif val in ['are flammable materials safely stored',
'are solid flammable materials safely stored',
'are possible sources of ignition appropriately safeguarded']:
        return "safe storage of flammable materials?"

    elif val in ['are emergency exits and escape routes clearly marked',
'are emergency exits and escape routes clearly marked and posted in the workplace',]:
        return "emergency exits clearly marked?"
    
    elif val in ['are there at least 2 possible exits for all workers, where required',
'are there enough adequate emergency exit doors',
'are there enough emergency exits',
'are there enough emergency exits? [differentiation question]',
'does the workplace have adequate stairways that can be used in case of emergency',
'does the workplace have at least one adequate window exit per floor']:
        return "enough emergency exits?"
    
    elif val in ['does the employer comply with size requirements for escape routes']:
        return "appropriate size of escape routes?"

    elif val in ['does the employer conduct at least one emergency drill per year',
'does the employer conduct periodic emergency drills',
'does the employer conduct periodic emergency drills? [differentiation question]',
'has the employer developed and trained workers on an emergency evacuation plan',
'has the employer elaborated and implemented an emergency plan',]:
        return "emergnecy drill/train/plan?"

    elif "fire" in val:
        return "appropriate fire-fighting stratgies?"
# 
    else:
        return val


print(cleanQLabel('are escape routes free of obstruction'))