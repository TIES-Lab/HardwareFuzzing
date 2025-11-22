import os
import re


def CFG_Node_brkdwn(line: str) -> tuple:
    CFG_pattern = re.compile(r"\s*(.+)\s*::\s*(.+)\s*::\s*(.+)\s*::\s*(.+)\s*;\s*")
    line_lst: list[str] = list(re.findall(CFG_pattern, line)[0])
    branch: list[str] = [x.strip() for x in re.split(r",", line_lst[1])]
    statemnt: str = line_lst[2]
    statemnt_type: str = line_lst[3]
    return branch, statemnt, statemnt_type


def CFG_file_brkdown(lines: list[str]) -> list[str]:
    for iter_no, iter_line in enumerate(lines):
        brnch, sttmnt, sttmnt_type = CFG_Node_brkdwn(iter_line)
        lines[iter_no] = [brnch, sttmnt, sttmnt_type]

    return lines


def CFG_get_upper(lines: list[str], trgt: str, out_lst = None):
    get_node = []
    for iter_no, iter_line in enumerate(lines):
        if iter_line[1].strip() == trgt.strip():
            get_node = iter_line[0] if iter_line[2] == "A" else iter_line[0][:-1]
            out_lst.append(iter_line)
            break
    if get_node != []:
        for iter_no, iter_line in enumerate(lines):
            if iter_line[0] == get_node:
                out_lst = CFG_get_upper(lines, iter_line[1], out_lst)
                break
        return out_lst
    else:
        return out_lst




def main():
    # ==================================================================================================================
    # ================================================ Folder Location =================================================
    # ==================================================================================================================
    # root_loc: str = 'C:/Users/ssm220008/OneDrive - The University of Texas at Dallas/DAC_ext'
    root_loc: str = 'D:/DAC_ext'
    RTL_loc: str = 'SoC_1/All_RTL/'
    srch_rtl: str = os.path.join(root_loc, RTL_loc)
    CFG_fldr_loc: str = 'Extracted_CFG'
    CFG_loc: str = os.path.join(root_loc, CFG_fldr_loc)
    # ================================================ Folder Location =================================================

    tst_file: str = "memory_control.txt"
    with open(os.path.join(CFG_loc, tst_file), "r") as fl:
        lines = fl.readlines()
        fl.close()

    trget_node: str = "ccif.dwait[dsource] <== 0"
    brkn_dwn_lines: list[str] = CFG_file_brkdown(lines)
    outlst = CFG_get_upper(lines, trget_node)
    outlst = outlst[::-1]
    print(outlst)




if __name__ == "__main__":
    main()