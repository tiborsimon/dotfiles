

temp_list = []
for i in range(5, 10):
    if (i % 3) > 0:
        value = f"-{i}-"
        temp_list.append(value)

result_1 = temp_list[1]
print(result_1)

result_2 = [f"-{i}-" for i in range(5,10) if i % 3][1]
print(result_2)
