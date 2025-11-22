import os

def search_files(directory, keywords):
    for filename in os.listdir(directory):
        if filename.endswith('.sv'):
            with open(os.path.join(directory, filename), 'r') as file:
                contents = file.read()
                if all(keyword in contents for keyword in keywords):
                    print(f'Found {keywords} in {filename}')

search_files('D:/PhD/DAC_ext/SoC_2/UF-UTD Colab SoC #2/source', ['datapath'])