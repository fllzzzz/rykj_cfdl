package com.cf.parking.facade.bo;

import lombok.Data;
import lombok.experimental.Accessors;


/**
 * 摇号结果导出bo对象
 * @author
 * @date 2023/9/26
 */
@Data
@Accessors(chain = true)
public class LotteryResultExportBO {


    /** 期号 */
    private String batchNum;

    /** 车位数量 */
    private Long parkingAmount;

    /** 摇号轮数，多个间逗号间隔 */
    private String roundId;

    /** 轮数名称 */
    private String roundName;

    /** 报名时间 */
    private String applyTime;

    /** 有效时间 */
    private String validDate;

    /** 停车场名称 */
    private String parkingLotName;

    /** 停车场编号 */
    private String parkingLotCode;

    /** 用户id */
    private Long userId;

    /** 用户姓名 */
    private String userName;

}
