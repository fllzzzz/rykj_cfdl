package com.cf.parking.api.response;

import cn.afterturn.easypoi.excel.annotation.Excel;
import io.swagger.annotations.ApiModel;
import lombok.Data;

/**
 * @author
 * @date 2023/9/27
 */
@Data
@ApiModel(description = "停车场分配人员导出结果")
public class LotteryRuleAssignExportRsp {

    /** 停车场名称 */
    @Excel(name = "停车场", orderNum = "1")
    private String parkingLotRegion;

    /** 人员名称 */
    @Excel(name = "姓名", orderNum = "2")
    private String name;

    /** 人员工号 */
    @Excel(name = "工号", orderNum = "3")
    private String code;


}
