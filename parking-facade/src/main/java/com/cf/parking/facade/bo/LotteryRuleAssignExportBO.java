package com.cf.parking.facade.bo;

import lombok.Data;
import lombok.experimental.Accessors;


/**
 * 摇号批次导出bo对象
 * @author
 * @date 2023/9/27
 */
@Data
@Accessors(chain = true)
public class LotteryRuleAssignExportBO {

    /** 停车场名称 */
    private String parkingLotRegion;

    /** 人员名称 */
    private String name;

    /** 人员工号 */
    private String code;

}
