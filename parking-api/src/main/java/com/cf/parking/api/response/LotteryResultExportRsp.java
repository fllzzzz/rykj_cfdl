package com.cf.parking.api.response;

import cn.afterturn.easypoi.excel.annotation.Excel;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;


/**
 * @author
 * @date 2023/9/26
 */
@Data
@ApiModel(description = "摇号结果导出结果")
public class LotteryResultExportRsp {

    /** 期号 */
    @ApiModelProperty(value = "期号")
    @Excel(name = "期号", orderNum = "1")
    private String batchNum;

    /** 车位数量 */
    @ApiModelProperty(value = "车位数量")
    @Excel(name = "车位数量", orderNum = "2")
    private Long parkingAmount;

    /** 轮数名称 */
    @ApiModelProperty(value = "轮数名称")
    @Excel(name = "轮数", orderNum = "5")
    private String roundName;

    /** 报名时间 */
    @ApiModelProperty(value = "报名时间")
    @Excel(name = "报名时间", orderNum = "3")
    private String applyTime;

    /** 有效时间 */
    @ApiModelProperty(value = "有效时间")
    @Excel(name = "有效时间", orderNum = "4")
    private String validDate;

    /** 停车场名称 */
    @ApiModelProperty(value = "停车场")
    @Excel(name = "停车场", orderNum = "6")
    private String parkingLotName;

    /** 用户姓名 */
    @ApiModelProperty(value = "用户")
    @Excel(name = "用户", orderNum = "7")
    private String userName;
}
